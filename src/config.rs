use crate::descriptors::InheritanceDescriptor;

use std::{net::SocketAddr, path::PathBuf, str::FromStr, time::Duration};

use miniscript::{bitcoin::Network, DescriptorPublicKey, ForEach, ForEachKey};

use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

fn deserialize_fromstr<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: FromStr,
    <T as FromStr>::Err: std::fmt::Display,
{
    let string = String::deserialize(deserializer)?;
    T::from_str(&string)
        .map_err(|e| de::Error::custom(format!("Error parsing descriptor '{}': '{}'", string, e)))
}

pub fn serialize_to_string<T: std::fmt::Display, S: Serializer>(
    field: T,
    s: S,
) -> Result<S::Ok, S::Error> {
    s.serialize_str(&field.to_string())
}

fn deserialize_duration<'de, D>(deserializer: D) -> Result<Duration, D::Error>
where
    D: Deserializer<'de>,
{
    let secs = u64::deserialize(deserializer)?;
    Ok(Duration::from_secs(secs))
}
pub fn serialize_duration<S: Serializer>(duration: &Duration, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_u64(duration.as_secs())
}

fn default_loglevel() -> log::LevelFilter {
    log::LevelFilter::Info
}

fn default_poll_interval() -> Duration {
    Duration::from_secs(30)
}

#[cfg(unix)]
fn default_daemon() -> bool {
    false
}

// TODO: separate Bitcoin config and bitcoind-specific config.
/// Everything we need to know for talking to bitcoind serenely
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct BitcoindConfig {
    /// Path to bitcoind's cookie file, to authenticate the RPC connection
    pub cookie_path: PathBuf,
    /// The IP:port bitcoind's RPC is listening on
    pub addr: SocketAddr,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct BitcoinConfig {
    /// The network we are operating on, one of "bitcoin", "testnet", "regtest", "signet"
    pub network: Network,
    /// The poll interval for the Bitcoin interface
    #[serde(
        deserialize_with = "deserialize_duration",
        serialize_with = "serialize_duration",
        default = "default_poll_interval"
    )]
    pub poll_interval_secs: Duration,
}

/// Static informations we require to operate
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Config {
    /// An optional custom data directory
    pub data_dir: Option<PathBuf>,
    /// Whether to daemonize the process
    #[cfg(unix)]
    #[serde(default = "default_daemon")]
    pub daemon: bool,
    /// What messages to log
    #[serde(
        deserialize_with = "deserialize_fromstr",
        serialize_with = "serialize_to_string",
        default = "default_loglevel"
    )]
    pub log_level: log::LevelFilter,
    /// The descriptor to use for sending/receiving coins
    #[serde(
        deserialize_with = "deserialize_fromstr",
        serialize_with = "serialize_to_string"
    )]
    pub main_descriptor: InheritanceDescriptor,
    /// Settings for the Bitcoin interface
    pub bitcoin_config: BitcoinConfig,
    /// Settings specific to bitcoind as the Bitcoin interface
    pub bitcoind_config: Option<BitcoindConfig>,
}

impl Config {
    pub fn data_dir(&self) -> Option<PathBuf> {
        self.data_dir
            .as_ref()
            .map(Clone::clone)
            .or_else(config_folder_path)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum ConfigError {
    DatadirNotFound,
    FileNotFound,
    ReadingFile(String),
    UnexpectedDescriptor(InheritanceDescriptor),
    Unexpected(String),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Self::DatadirNotFound => write!(f, "Could not locate the configuration directory."),
            Self::FileNotFound => write!(f, "Could not locate the configuration file."),
            Self::ReadingFile(e) => write!(f, "Failed to read configuration file: {}", e),
            Self::UnexpectedDescriptor(desc) => write!(
                f,
                "Unexpected descriptor '{}'. We only support wsh() descriptors for now.",
                desc
            ),
            Self::Unexpected(e) => write!(f, "Configuration error: {}", e),
        }
    }
}

impl From<std::io::Error> for ConfigError {
    fn from(e: std::io::Error) -> Self {
        match e.kind() {
            std::io::ErrorKind::NotFound => Self::FileNotFound,
            _ => Self::ReadingFile(e.to_string()),
        }
    }
}

impl std::error::Error for ConfigError {}

/// Get the absolute path to the minisafe configuration folder.
///
/// It's a "minisafe/<network>/" directory in the XDG standard configuration directory for
/// all OSes but Linux-based ones, for which it's `~/.minisafe/<network>/`.
/// There is only one config file at `minisafe/config.toml`, which specifies the network.
/// Rationale: we want to have the database, RPC socket, etc.. in the same folder as the
/// configuration file but for Linux the XDG specifoes a data directory (`~/.local/share/`)
/// different from the configuration one (`~/.config/`).
pub fn config_folder_path() -> Option<PathBuf> {
    #[cfg(target_os = "linux")]
    let configs_dir = dirs::home_dir();

    #[cfg(not(target_os = "linux"))]
    let configs_dir = dirs::config_dir();

    if let Some(mut path) = configs_dir {
        #[cfg(target_os = "linux")]
        path.push(".minisafe");

        #[cfg(not(target_os = "linux"))]
        path.push("Minisafe");

        return Some(path);
    }

    None
}

fn config_file_path() -> Option<PathBuf> {
    config_folder_path().map(|mut path| {
        path.push("minisafe.toml");
        path
    })
}

impl Config {
    /// Get our static configuration out of a mandatory configuration file.
    ///
    /// We require all settings to be set in the configuration file, and only in the configuration
    /// file. We don't allow to set them via the command line or environment variables to avoid a
    /// futile duplication.
    pub fn from_file(custom_path: Option<PathBuf>) -> Result<Config, ConfigError> {
        let config_file =
            custom_path.unwrap_or(config_file_path().ok_or_else(|| ConfigError::DatadirNotFound)?);

        let config = toml::from_slice::<Config>(&std::fs::read(&config_file)?)
            .map_err(|e| ConfigError::ReadingFile(format!("Parsing configuration file: {}", e)))?;
        config.check()?;

        Ok(config)
    }

    /// Make sure the settings are sane.
    pub fn check(&self) -> Result<(), ConfigError> {
        // Check the network of the xpubs in the descriptors
        let expected_network = match self.bitcoin_config.network {
            Network::Bitcoin => Network::Bitcoin,
            _ => Network::Testnet,
        };
        let unexpected_net = self.main_descriptor.as_inner().for_each_key(|pkpkh| {
            let xpub = match pkpkh {
                // For DescriptorPublicKey, Pk::Hash == Self.
                ForEach::Key(xpub) => xpub,
                ForEach::Hash(xpub) => xpub,
            };
            if let DescriptorPublicKey::XPub(xpub) = xpub {
                xpub.xkey.network != expected_network
            } else {
                false
            }
        });
        if unexpected_net {
            return Err(ConfigError::Unexpected(format!(
                "Our bitcoin network is {} but one xpub is not for network {}",
                self.bitcoin_config.network, expected_network
            )));
        }

        // TODO: check the semantics of the main descriptor

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{config_file_path, Config};

    // Test the format of the configuration file
    #[test]
    fn toml_config() {
        // A valid config
        let toml_str = r#"
            data_dir = "/home/wizardsardine/custom/folder/"
            daemon = false
            log_level = "debug"
            main_descriptor = "wsh(andor(pk(tpubDEN9WSToTyy9ZQfaYqSKfmVqmq1VVLNtYfj3Vkqh67et57eJ5sTKZQBkHqSwPUsoSskJeaYnPttHe2VrkCsKA27kUaN9SDc5zhqeLzKa1rr/*),older(10000),pk(tpubD8LYfn6njiA2inCoxwM7EuN3cuLVcaHAwLYeups13dpevd3nHLRdK9NdQksWXrhLQVxcUZRpnp5CkJ1FhE61WRAsHxDNAkvGkoQkAeWDYjV/*)))#y5wcna2d"

            [bitcoin_config]
            network = "bitcoin"
            poll_interval_secs = 18

            [bitcoind_config]
            cookie_path = "/home/user/.bitcoin/.cookie"
            addr = "127.0.0.1:8332"
            "#.trim_start().replace("            ", "");
        toml::from_str::<Config>(&toml_str).expect("Deserializing toml_str");

        // A valid, round-tripping, config
        let toml_str = r#"
            data_dir = '/home/wizardsardine/custom/folder/'
            daemon = false
            log_level = 'TRACE'
            main_descriptor = 'wsh(andor(pk(tpubDEN9WSToTyy9ZQfaYqSKfmVqmq1VVLNtYfj3Vkqh67et57eJ5sTKZQBkHqSwPUsoSskJeaYnPttHe2VrkCsKA27kUaN9SDc5zhqeLzKa1rr/*),older(10000),pk(tpubD8LYfn6njiA2inCoxwM7EuN3cuLVcaHAwLYeups13dpevd3nHLRdK9NdQksWXrhLQVxcUZRpnp5CkJ1FhE61WRAsHxDNAkvGkoQkAeWDYjV/*)))#y5wcna2d'

            [bitcoin_config]
            network = 'bitcoin'
            poll_interval_secs = 18

            [bitcoind_config]
            cookie_path = '/home/user/.bitcoin/.cookie'
            addr = '127.0.0.1:8332'
            "#.trim_start().replace("            ", "");
        let parsed = toml::from_str::<Config>(&toml_str).expect("Deserializing toml_str");
        let serialized = toml::to_string_pretty(&parsed).expect("Serializing to toml");
        #[cfg(unix)] // On non-UNIX there is no 'daemon' member.
        assert_eq!(toml_str, serialized);

        // Invalid desc checksum
        let toml_str = r#"
            daemon = false
            log_level = "trace"
            data_dir = "/home/wizardsardine/custom/folder/"

            main_descriptor = "wsh(andor(pk(tpubDEN9WSToTyy9ZQfaYqSKfmVqmq1VVLNtYfj3Vkqh67et57eJ5sTKZQBkHqSwPUsoSskJeaYnPttHe2VrkCsKA27kUaN9SDc5zhqeLzKa1rr/*),older(10000),pk(tpubD8LYfn6njiA2inCoxwM7EuN3cuLVcaHAwLYeups13dpevd3nHLRdK9NdQksWXrhLQVxcUZRpnp5CkJ1FhE61WRAsHxDNAkvGkoQkAeWDYjV/*)))#y5wcna2e"

            [bitcoin_config]
            network = "bitcoin"
            poll_interval_secs = 18

            [bitcoind_config]
            cookie_path = "/home/user/.bitcoin/.cookie"
            addr = "127.0.0.1:8332"
        "#;
        let config_res: Result<Config, toml::de::Error> = toml::from_str(toml_str);
        config_res.expect_err("Deserializing an invalid toml_str");

        // Not enough parameters: missing the Bitcoin network
        let toml_str = r#"
            daemon = false
            log_level = "trace"
            data_dir = "/home/wizardsardine/custom/folder/"

            # The main descriptor semantics aren't checked, yet.
            main_descriptor = ""

            [bitcoin_config]
            poll_interval_secs = 18

            [bitcoind_config]
            cookie_path = "/home/user/.bitcoin/.cookie"
            addr = "127.0.0.1:8332"
        "#;
        let config_res: Result<Config, toml::de::Error> = toml::from_str(toml_str);
        config_res.expect_err("Deserializing an invalid toml_str");
    }

    #[test]
    fn config_directory() {
        let filepath = config_file_path().expect("Getting config file path");

        #[cfg(target_os = "linux")]
        {
            assert!(filepath.as_path().starts_with("/home/"));
            assert!(filepath.as_path().ends_with(".minisafe/minisafe.toml"));
        }

        #[cfg(target_os = "macos")]
        assert!(filepath
            .as_path()
            .ends_with("Library/Application Support/Minisafe/minisafe.toml"));

        #[cfg(target_os = "windows")]
        assert!(filepath
            .as_path()
            .ends_with(r#"AppData\Roaming\Minisafe\minisafe.toml"#));
    }
}
