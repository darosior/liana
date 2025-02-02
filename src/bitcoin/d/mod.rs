///! Implementation of the Bitcoin interface using bitcoind.
///!
///! We use the RPC interface and a watchonly descriptor wallet.
use crate::{bitcoin::BlockChainTip, config, descriptors::InheritanceDescriptor};

use std::{collections::HashSet, convert::TryInto, fs, io, str::FromStr, time::Duration};

use jsonrpc::{
    arg,
    client::Client,
    simple_http::{self, SimpleHttpTransport},
};
use miniscript::bitcoin;

use serde_json::Value as Json;

// If bitcoind takes more than 3 minutes to answer one of our queries, fail.
const RPC_SOCKET_TIMEOUT: u64 = 180;

// Number of retries the client is allowed to do in case of timeout or i/o error
// while communicating with the bitcoin daemon.
// A retry happens every 1 second, this makes us give up after one minute.
const BITCOIND_RETRY_LIMIT: usize = 60;

// The minimum bitcoind version that can be used with revaultd.
const MIN_BITCOIND_VERSION: u64 = 239900;

/// An error in the bitcoind interface.
#[derive(Debug)]
pub enum BitcoindError {
    CookieFile(io::Error),
    /// Bitcoind server error.
    Server(jsonrpc::error::Error),
    /// They replied to a batch request omitting some responses.
    BatchMissingResponse,
    WalletCreation(String),
    DescriptorImport(String),
    WalletLoading(String),
    MissingOrTooManyWallet,
    InvalidVersion(u64),
    NetworkMismatch(String /*config*/, String /*bitcoind*/),
    MissingDescriptor,
}

impl BitcoindError {
    /// Is bitcoind just starting ?
    pub fn is_warming_up(&self) -> bool {
        match self {
            // https://github.com/bitcoin/bitcoin/blob/dca80ffb45fcc8e6eedb6dc481d500dedab4248b/src/rpc/protocol.h#L49
            BitcoindError::Server(jsonrpc::error::Error::Rpc(jsonrpc::error::RpcError {
                code,
                ..
            })) => *code == -28,
            _ => false,
        }
    }
}

impl std::fmt::Display for BitcoindError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BitcoindError::CookieFile(e) => write!(f, "Reading bitcoind cookie file: {}", e),
            BitcoindError::Server(ref e) => write!(f, "Bitcoind RPC server error: {}", e),
            BitcoindError::BatchMissingResponse => write!(
                f,
                "Bitcoind server replied without enough responses to our batched request"
            ),
            BitcoindError::WalletCreation(s) => write!(f, "Error creating watchonly wallet: {}", s),
            BitcoindError::DescriptorImport(s) => write!(
                f,
                "Error importing descriptor. Response from bitcoind: '{}'",
                s
            ),
            BitcoindError::WalletLoading(s) => {
                write!(f, "Error when loading watchonly wallet: '{}'.", s)
            }
            BitcoindError::InvalidVersion(v) => {
                write!(
                    f,
                    "Invalid bitcoind version '{}', minimum supported is '{}'.",
                    v, MIN_BITCOIND_VERSION
                )
            }
            BitcoindError::NetworkMismatch(conf_net, bitcoind_net) => {
                write!(
                    f,
                    "Network mismatch. We are supposed to run on '{}' but bitcoind is on '{}'.",
                    conf_net, bitcoind_net
                )
            }
            BitcoindError::MissingOrTooManyWallet => {
                write!(
                    f,
                    "No, or too many, watchonly wallet(s) loaded on bitcoind."
                )
            }
            BitcoindError::MissingDescriptor => {
                write!(f, "The watchonly wallet loaded on bitcoind does not have the main descriptor imported.")
            }
        }
    }
}

impl std::error::Error for BitcoindError {}

impl From<jsonrpc::error::Error> for BitcoindError {
    fn from(e: jsonrpc::error::Error) -> Self {
        Self::Server(e)
    }
}

impl From<simple_http::Error> for BitcoindError {
    fn from(e: simple_http::Error) -> Self {
        jsonrpc::error::Error::Transport(Box::new(e)).into()
    }
}

pub struct BitcoinD {
    node_client: Client,
    watchonly_client: Client,
    watchonly_wallet_path: String,
    /// How many times we'll retry upon failure to send a request.
    retries: usize,
}

macro_rules! params {
    ($($param:expr),* $(,)?) => {
        [
            $(
                arg($param),
            )*
        ]
    };
}

impl BitcoinD {
    /// Create a new bitcoind interface. This tests the connection to bitcoind and disables retries
    /// on failure to send a request.
    pub fn new(
        config: &config::BitcoindConfig,
        watchonly_wallet_path: String,
    ) -> Result<BitcoinD, BitcoindError> {
        let cookie_string =
            fs::read_to_string(&config.cookie_path).map_err(BitcoindError::CookieFile)?;

        // Create a dummy client with a low timeout first to test the connection
        let dummy_node_client = Client::with_transport(
            SimpleHttpTransport::builder()
                .url(&config.addr.to_string())
                .map_err(BitcoindError::from)?
                .timeout(Duration::from_secs(3))
                .cookie_auth(cookie_string.clone())
                .build(),
        );
        let req = dummy_node_client.build_request("echo", &[]);
        dummy_node_client.send_request(req.clone())?;

        let node_client = Client::with_transport(
            SimpleHttpTransport::builder()
                .url(&config.addr.to_string())
                .map_err(BitcoindError::from)?
                .timeout(Duration::from_secs(RPC_SOCKET_TIMEOUT))
                .cookie_auth(cookie_string.clone())
                .build(),
        );

        // Create a dummy client with a low timeout first to test the connection
        let url = format!("http://{}/wallet/{}", config.addr, watchonly_wallet_path);
        let dummy_wo_client = Client::with_transport(
            SimpleHttpTransport::builder()
                .url(&url)
                .map_err(BitcoindError::from)?
                .timeout(Duration::from_secs(3))
                .cookie_auth(cookie_string.clone())
                .build(),
        );
        let req = dummy_wo_client.build_request("echo", &[]);
        dummy_wo_client.send_request(req.clone())?;

        let watchonly_url = format!("http://{}/wallet/{}", config.addr, watchonly_wallet_path);
        let watchonly_client = Client::with_transport(
            SimpleHttpTransport::builder()
                .url(&watchonly_url)
                .map_err(BitcoindError::from)?
                .timeout(Duration::from_secs(RPC_SOCKET_TIMEOUT))
                .cookie_auth(cookie_string.clone())
                .build(),
        );

        Ok(BitcoinD {
            node_client,
            watchonly_client,
            watchonly_wallet_path,
            retries: 0,
        })
    }

    /// Set how many times we'll retry a failed request. If passed None will set to default.
    pub fn with_retry_limit(mut self, retry_limit: Option<usize>) -> Self {
        self.retries = retry_limit.unwrap_or(BITCOIND_RETRY_LIMIT);
        self
    }

    /// Wrapper to retry a request sent to bitcoind upon IO failure
    /// according to the configured number of retries.
    fn retry<T, R: Fn() -> Result<T, BitcoindError>>(
        &self,
        request: R,
    ) -> Result<T, BitcoindError> {
        let mut error: Option<BitcoindError> = None;
        for i in 0..self.retries + 1 {
            match request() {
                Ok(res) => return Ok(res),
                Err(e) => {
                    if e.is_warming_up() {
                        error = Some(e)
                    } else if let BitcoindError::Server(jsonrpc::Error::Transport(ref err)) = e {
                        match err.downcast_ref::<simple_http::Error>() {
                            Some(simple_http::Error::Timeout)
                            | Some(simple_http::Error::SocketError(_))
                            | Some(simple_http::Error::HttpErrorCode(503)) => {
                                std::thread::sleep(Duration::from_secs(1));
                                log::debug!("Retrying RPC request to bitcoind: attempt #{}", i);
                                error = Some(e);
                            }
                            _ => return Err(e),
                        }
                    } else {
                        return Err(e);
                    }
                }
            }
        }

        Err(error.expect("Always set if we reach this point"))
    }

    fn make_request<'a, 'b>(
        &self,
        client: &Client,
        method: &'a str,
        params: &'b [Box<serde_json::value::RawValue>],
    ) -> Result<Json, BitcoindError> {
        self.retry(|| {
            let req = client.build_request(method, params);
            log::trace!("Sending to bitcoind: {:#?}", req);
            match client.send_request(req) {
                Ok(resp) => {
                    let res = resp.result().map_err(BitcoindError::Server)?;
                    log::trace!("Got from bitcoind: {:#?}", res);

                    return Ok(res);
                }
                Err(e) => Err(BitcoindError::Server(e)),
            }
        })
    }

    fn make_node_request(&self, method: &str, params: &[Box<serde_json::value::RawValue>]) -> Json {
        self.make_request(&self.node_client, method, params)
            .expect("We must not fail to make a request for more than a minute")
    }

    fn make_fallible_node_request(
        &self,
        method: &str,
        params: &[Box<serde_json::value::RawValue>],
    ) -> Result<Json, BitcoindError> {
        self.make_request(&self.node_client, method, params)
    }

    fn make_wallet_request(
        &self,
        method: &str,
        params: &[Box<serde_json::value::RawValue>],
    ) -> Json {
        self.make_request(&self.watchonly_client, method, params)
            .expect("We must not fail to make a request for more than a minute")
    }

    fn make_faillible_wallet_request(
        &self,
        method: &str,
        params: &[Box<serde_json::value::RawValue>],
    ) -> Result<Json, BitcoindError> {
        self.make_request(&self.watchonly_client, method, params)
    }

    fn get_bitcoind_version(&self) -> u64 {
        self.make_node_request("getnetworkinfo", &[])
            .get("version")
            .map(Json::as_u64)
            .flatten()
            .expect("Missing or invalid 'version' in 'getnetworkinfo' result?")
    }

    fn get_network_bip70(&self) -> String {
        self.make_node_request("getblockchaininfo", &[])
            .get("chain")
            .map(Json::as_str)
            .flatten()
            .expect("Missing or invalid 'chain' in 'getblockchaininfo' result?")
            .to_string()
    }

    fn list_wallets(&self) -> Vec<String> {
        self.make_node_request("listwallets", &[])
            .as_array()
            .expect("API break, 'listwallets' didn't return an array.")
            .iter()
            .map(|json_str| {
                json_str
                    .as_str()
                    .expect("API break: 'listwallets' contains a non-string value")
                    .to_string()
            })
            .collect()
    }

    fn unload_wallet(&self, wallet_path: String) -> Option<String> {
        self.make_node_request("unloadwallet", &params!(Json::String(wallet_path),))
            .get("warning")
            .expect("No 'warning' in 'unloadwallet' response?")
            .as_str()
            .and_then(|w| {
                if w.is_empty() {
                    None
                } else {
                    Some(w.to_string())
                }
            })
    }

    fn create_wallet(&self, wallet_path: String) -> Option<String> {
        let res = self.make_node_request(
            "createwallet",
            &params!(
                Json::String(wallet_path),
                Json::Bool(true), // watchonly
                Json::Bool(true), // blank
            ),
        );

        if let Some(warning) = res.get("warning").map(Json::as_str).flatten() {
            if !warning.is_empty() {
                return Some(warning.to_string());
            }
        }
        if res.get("name").is_none() {
            return Some("Unknown error when create watchonly wallet".to_string());
        }

        None
    }

    // TODO: rescan feature will probably need another timestamp than 'now'
    fn import_descriptor(&self, descriptor: &InheritanceDescriptor) -> Option<String> {
        let descriptors = vec![serde_json::json!({
            "desc": descriptor.to_string(),
            "timestamp": "now",
            "active": false,
        })];

        let res = self.make_wallet_request("importdescriptors", &params!(Json::Array(descriptors)));
        let all_succeeded = res
            .as_array()
            .map(|results| {
                results.iter().all(|res| {
                    res.get("success")
                        .map(Json::as_bool)
                        .flatten()
                        .unwrap_or(false)
                })
            })
            .unwrap_or(false);
        if all_succeeded {
            None
        } else {
            Some(res.to_string())
        }
    }

    fn list_descriptors(&self) -> Vec<String> {
        self.make_wallet_request("listdescriptors", &[])
            .get("descriptors")
            .and_then(Json::as_array)
            .expect("Missing or invalid 'descriptors' field in 'listdescriptors' response")
            .iter()
            .map(|elem| {
                elem.get("desc")
                    .and_then(Json::as_str)
                    .expect(
                        "Missing or invalid 'desc' field in 'listdescriptors' response's entries",
                    )
                    .to_string()
            })
            .collect::<Vec<String>>()
    }

    /// Create the watchonly wallet on bitcoind, and import it the main descriptor.
    pub fn create_watchonly_wallet(
        &self,
        main_descriptor: &InheritanceDescriptor,
    ) -> Result<(), BitcoindError> {
        // Remove any leftover. This can happen if we delete the watchonly wallet but don't restart
        // bitcoind.
        while self.list_wallets().contains(&self.watchonly_wallet_path) {
            log::info!("Found a leftover watchonly wallet loaded on bitcoind. Removing it.");
            if let Some(e) = self.unload_wallet(self.watchonly_wallet_path.clone()) {
                log::error!(
                    "Unloading wallet '{}': '{}'",
                    &self.watchonly_wallet_path,
                    e
                );
            }
        }

        // Now create the wallet and import the main descriptor.
        if let Some(err) = self.create_wallet(self.watchonly_wallet_path.clone()) {
            return Err(BitcoindError::WalletCreation(err));
        }
        if let Some(err) = self.import_descriptor(main_descriptor) {
            return Err(BitcoindError::DescriptorImport(err));
        }

        Ok(())
    }

    /// Try to load the watchonly wallet in bitcoind. It will continue on error (since it's
    /// likely the wallet is just already loaded) and log it as info instead.
    pub fn try_load_watchonly_wallet(&self) {
        if let Err(e) = self.make_fallible_node_request(
            "loadwallet",
            &params!(Json::String(self.watchonly_wallet_path.clone()),),
        ) {
            log::info!("Got error '{}' while trying to load watchonly on bitcoind. It is possibly already loaded.", e);
        }
    }

    /// Perform various sanity checks on the bitcoind instance.
    pub fn sanity_check(
        &self,
        main_descriptor: &InheritanceDescriptor,
        config_network: bitcoin::Network,
    ) -> Result<(), BitcoindError> {
        // Check the minimum supported bitcoind version
        let version = self.get_bitcoind_version();
        if version < MIN_BITCOIND_VERSION {
            return Err(BitcoindError::InvalidVersion(version));
        }

        // Check bitcoind is running on the right network
        let bitcoind_net = self.get_network_bip70();
        let bip70_net = match config_network {
            bitcoin::Network::Bitcoin => "main",
            bitcoin::Network::Testnet => "test",
            bitcoin::Network::Regtest => "regtest",
            bitcoin::Network::Signet => "signet",
        };
        if bitcoind_net != bip70_net {
            return Err(BitcoindError::NetworkMismatch(
                bip70_net.to_string(),
                bitcoind_net,
            ));
        }

        // Check our watchonly wallet is loaded
        if self
            .list_wallets()
            .iter()
            .filter(|s| s == &&self.watchonly_wallet_path)
            .count()
            != 1
        {
            return Err(BitcoindError::MissingOrTooManyWallet);
        }

        // Check our main descriptor is imported in this wallet.
        if !self
            .list_descriptors()
            .contains(&main_descriptor.to_string())
        {
            return Err(BitcoindError::MissingDescriptor);
        }

        Ok(())
    }

    fn block_chain_info(&self) -> Json {
        self.make_node_request("getblockchaininfo", &[])
    }

    pub fn sync_progress(&self) -> f64 {
        // TODO: don't harass revaultd, be smarter like in revaultd.
        roundup_progress(
            self.block_chain_info()
                .get("verificationprogress")
                .and_then(Json::as_f64)
                .expect("No valid 'verificationprogress' in getblockchaininfo response?"),
        )
    }

    pub fn chain_tip(&self) -> BlockChainTip {
        // We use getblockchaininfo to avoid a race between getblockcount and getblockhash
        let chain_info = self.block_chain_info();
        let hash = bitcoin::BlockHash::from_str(
            chain_info
                .get("bestblockhash")
                .and_then(Json::as_str)
                .expect("No valid 'bestblockhash' in 'getblockchaininfo' response?"),
        )
        .expect("Invalid blockhash from bitcoind?");
        let height: i32 = chain_info
            .get("blocks")
            .and_then(Json::as_i64)
            .expect("No valid 'blocks' in 'getblockchaininfo' response?")
            .try_into()
            .expect("Must fit by Bitcoin consensus");

        BlockChainTip { hash, height }
    }

    pub fn get_block_hash(&self, height: i32) -> Option<bitcoin::BlockHash> {
        Some(
            self.make_fallible_node_request("getblockhash", &params!(Json::Number(height.into()),))
                .ok()?
                .as_str()
                .and_then(|s| bitcoin::BlockHash::from_str(s).ok())
                .expect("bitcoind must send valid block hashes"),
        )
    }

    pub fn list_since_block(&self, block_hash: &bitcoin::BlockHash) -> LSBlockRes {
        self.make_wallet_request(
            "listsinceblock",
            &params!(Json::String(block_hash.to_string()),),
        )
        .into()
    }

    pub fn get_transaction(&self, txid: &bitcoin::Txid) -> Option<GetTxRes> {
        // TODO: Maybe assert we got a -5 error, and not any other kind of error?
        self.make_faillible_wallet_request(
            "gettransaction",
            &params!(Json::String(txid.to_string())),
        )
        .ok()
        .map(|res| res.into())
    }

    /// Efficient check that a coin is spent.
    pub fn is_spent(&self, op: &bitcoin::OutPoint) -> bool {
        // The result of gettxout is empty if the outpoint is spent.
        self.make_node_request(
            "gettxout",
            &params!(
                Json::String(op.txid.to_string()),
                Json::Number(op.vout.into())
            ),
        )
        .get("bestblock")
        .is_none()
    }

    /// So, bitcoind has no API for getting the transaction spending a wallet UTXO. Instead we are
    /// therefore using a rather convoluted way to get it the other way around, since the spending
    /// transaction is actually *part of the wallet transactions*.
    /// So, what we do there is listing all outgoing transactions of the wallet since the last poll
    /// and iterating through each of those to check if it spends the transaction we are interested
    /// in (requiring an other RPC call for each!!).
    pub fn get_spender_txid(&self, spent_outpoint: &bitcoin::OutPoint) -> Option<bitcoin::Txid> {
        // Get the hash of the block parent of the spent transaction's block.
        let req = self.make_wallet_request(
            "gettransaction",
            &params!(Json::String(spent_outpoint.txid.to_string())),
        );
        let spent_tx_height = match req.get("blockheight").and_then(Json::as_i64) {
            Some(h) => h,
            // FIXME: we assume it's confirmed. If we were to change the logic in the poller, we'd
            // need to handle it here.
            None => return None,
        };
        let block_hash = if let Ok(res) = self.make_fallible_node_request(
            "getblockhash",
            &params!(Json::Number((spent_tx_height - 1).into())),
        ) {
            res.as_str()
                .expect("'getblockhash' result isn't a string")
                .to_string()
        } else {
            // Possibly a race.
            return None;
        };

        // Now we can get all transactions related to us since the spent transaction confirmed.
        // We'll use it to locate the spender.
        let lsb_res =
            self.make_wallet_request("listsinceblock", &params!(Json::String(block_hash)));
        let transactions = lsb_res
            .get("transactions")
            .and_then(Json::as_array)
            .expect("tx array must be there");

        // Get the spent txid to ignore the entries about this transaction
        let spent_txid = spent_outpoint.txid.to_string();
        // We use a cache to avoid needless iterations, since listsinceblock returns an entry
        // per transaction output, not per transaction.
        let mut visited_txs = HashSet::with_capacity(transactions.len());
        for transaction in transactions {
            if transaction.get("category").and_then(Json::as_str) != Some("send") {
                continue;
            }

            let spending_txid = transaction
                .get("txid")
                .and_then(Json::as_str)
                .expect("A valid txid must be present");
            if visited_txs.contains(&spending_txid) || &spent_txid == spending_txid {
                continue;
            } else {
                visited_txs.insert(spending_txid);
            }

            let gettx_res = self.make_wallet_request(
                "gettransaction",
                &params!(
                    Json::String(spending_txid.to_string()),
                    Json::Bool(true), // watchonly
                    Json::Bool(true)  // verbose
                ),
            );
            let vin = gettx_res
                .get("decoded")
                .and_then(|d| d.get("vin").and_then(Json::as_array))
                .expect("A valid vin array must be present");

            for input in vin {
                let txid = input
                    .get("txid")
                    .and_then(Json::as_str)
                    .and_then(|t| bitcoin::Txid::from_str(t).ok())
                    .expect("A valid txid must be present");
                let vout = input
                    .get("vout")
                    .and_then(Json::as_u64)
                    .expect("A valid vout must be present") as u32;
                let input_outpoint = bitcoin::OutPoint { txid, vout };

                if spent_outpoint == &input_outpoint {
                    return bitcoin::Txid::from_str(spending_txid)
                        .map(Some)
                        .expect("Must be a valid txid");
                }
            }
        }

        None
    }
}
// Bitcoind uses a guess for the value of verificationprogress. It will eventually get to
// be 1, and we want to be less conservative.
fn roundup_progress(progress: f64) -> f64 {
    let precision = 10u64.pow(5) as f64;
    let progress_rounded = (progress * precision + 1.0) as u64;

    if progress_rounded >= precision as u64 {
        1.0
    } else {
        (progress_rounded as f64 / precision) as f64
    }
}

/// A 'received' entry in the 'listsinceblock' result.
#[derive(Debug, Clone)]
pub struct LSBlockEntry {
    pub outpoint: bitcoin::OutPoint,
    pub amount: bitcoin::Amount,
    pub block_height: Option<i32>,
    pub address: bitcoin::Address,
}

impl From<&Json> for LSBlockEntry {
    fn from(json: &Json) -> LSBlockEntry {
        let txid = json
            .get("txid")
            .and_then(Json::as_str)
            .and_then(|s| bitcoin::Txid::from_str(s).ok())
            .expect("bitcoind can't give a bad block hash");
        let vout = json
            .get("vout")
            .and_then(Json::as_u64)
            .expect("bitcoind can't give a bad vout") as u32;
        let outpoint = bitcoin::OutPoint { txid, vout };

        // Must be a received entry, hence not negative.
        let amount = json
            .get("amount")
            .and_then(Json::as_f64)
            .and_then(|a| bitcoin::Amount::from_btc(a).ok())
            .expect("bitcoind won't give us a bad amount");
        let block_height = json
            .get("blockheight")
            .and_then(Json::as_i64)
            .map(|bh| bh as i32);

        let address = json
            .get("address")
            .and_then(Json::as_str)
            .and_then(|s| bitcoin::Address::from_str(s).ok())
            .expect("bitcoind can't give a bad address");

        LSBlockEntry {
            outpoint,
            amount,
            block_height,
            address,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LSBlockRes {
    pub received_coins: Vec<LSBlockEntry>,
}

impl From<Json> for LSBlockRes {
    fn from(json: Json) -> LSBlockRes {
        let received_coins = json
            .get("transactions")
            .and_then(Json::as_array)
            .expect("Array must be present")
            .into_iter()
            .filter_map(|j| {
                if j.get("category")
                    .and_then(Json::as_str)
                    .expect("must be present")
                    == "receive"
                {
                    let lsb_entry: LSBlockEntry = j.into();
                    Some(lsb_entry)
                } else {
                    None
                }
            })
            .collect();

        LSBlockRes { received_coins }
    }
}

#[derive(Debug, Clone)]
pub struct GetTxRes {
    pub conflicting_txs: Vec<bitcoin::Txid>,
    pub block_height: Option<i32>,
    pub block_time: Option<u32>,
}

impl From<Json> for GetTxRes {
    fn from(json: Json) -> GetTxRes {
        let block_height = json
            .get("blockheight")
            .and_then(Json::as_i64)
            .map(|bh| bh as i32);
        let block_time = json
            .get("blocktime")
            .and_then(Json::as_u64)
            .map(|bt| bt as u32);
        let conflicting_txs = json
            .get("walletconflicts")
            .and_then(Json::as_array)
            .map(|array| {
                array
                    .into_iter()
                    .map(|v| {
                        bitcoin::Txid::from_str(v.as_str().expect("wrong json format")).unwrap()
                    })
                    .collect()
            });

        GetTxRes {
            conflicting_txs: conflicting_txs.unwrap_or_default(),
            block_height,
            block_time,
        }
    }
}
