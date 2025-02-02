from fixtures import *
from test_framework.serializations import PSBT
from test_framework.utils import wait_for, COIN


def test_getinfo(minisafed):
    res = minisafed.rpc.getinfo()
    assert res["version"] == "0.1"
    assert res["network"] == "regtest"
    assert res["blockheight"] == 101
    assert res["sync"] == 1.0
    assert "main" in res["descriptors"]


def test_getaddress(minisafed):
    res = minisafed.rpc.getnewaddress()
    assert "address" in res
    # We'll get a new one at every call
    assert res["address"] != minisafed.rpc.getnewaddress()["address"]


def test_listcoins(minisafed, bitcoind):
    # Initially empty
    res = minisafed.rpc.listcoins()
    assert "coins" in res
    assert len(res["coins"]) == 0

    # If we send a coin, we'll get a new entry. Note we monitor for unconfirmed
    # funds as well.
    addr = minisafed.rpc.getnewaddress()["address"]
    txid = bitcoind.rpc.sendtoaddress(addr, 1)
    wait_for(lambda: len(minisafed.rpc.listcoins()["coins"]) == 1)
    res = minisafed.rpc.listcoins()["coins"]
    assert txid == res[0]["outpoint"][:64]
    assert res[0]["amount"] == 1 * COIN
    assert res[0]["block_height"] is None

    # If the coin gets confirmed, it'll be marked as such.
    bitcoind.generate_block(1, wait_for_mempool=txid)
    block_height = bitcoind.rpc.getblockcount()
    wait_for(
        lambda: minisafed.rpc.listcoins()["coins"][0]["block_height"] == block_height
    )


def test_jsonrpc_server(minisafed, bitcoind):
    """Test passing parameters as a list or a mapping."""
    addr = minisafed.rpc.getnewaddress()["address"]
    bitcoind.rpc.sendtoaddress(addr, 1)
    wait_for(lambda: len(minisafed.rpc.listcoins()["coins"]) == 1)
    outpoints = [minisafed.rpc.listcoins()["coins"][0]["outpoint"]]
    destinations = {
        bitcoind.rpc.getnewaddress(): 20_000,
    }
    res = minisafed.rpc.createspend(outpoints, destinations, 18)
    assert "psbt" in res
    res = minisafed.rpc.createspend(
        outpoints=outpoints, destinations=destinations, feerate=18
    )
    assert "psbt" in res


def test_create_spend(minisafed, bitcoind):
    # Receive a number of coins in different blocks on different addresses, and
    # one more on the same address.
    for _ in range(15):
        addr = minisafed.rpc.getnewaddress()["address"]
        txid = bitcoind.rpc.sendtoaddress(addr, 0.01)
        bitcoind.generate_block(1, wait_for_mempool=txid)
    txid = bitcoind.rpc.sendtoaddress(addr, 0.3556)
    bitcoind.generate_block(1, wait_for_mempool=txid)

    # Stop the daemon, should be a no-op
    minisafed.stop()
    minisafed.start()

    # Now create a transaction spending all those coins to a few addresses
    outpoints = [c["outpoint"] for c in minisafed.rpc.listcoins()["coins"]]
    destinations = {
        bitcoind.rpc.getnewaddress(): 200_000,
        bitcoind.rpc.getnewaddress(): 400_000,
        bitcoind.rpc.getnewaddress(): 1_000_000,
    }
    res = minisafed.rpc.createspend(outpoints, destinations, 18)
    assert "psbt" in res

    # The transaction must contain a change output.
    spend_psbt = PSBT()
    spend_psbt.deserialize(res["psbt"])
    assert len(spend_psbt.outputs) == 4
    assert len(spend_psbt.tx.vout) == 4

    # We can sign it and broadcast it.
    signed_tx_hex = minisafed.sign_psbt(spend_psbt)
    bitcoind.rpc.sendrawtransaction(signed_tx_hex)


def test_list_spend(minisafed, bitcoind):
    # Start by creating two conflicting Spend PSBTs. The first one will have a change
    # output but not the second one.
    addr = minisafed.rpc.getnewaddress()["address"]
    value_a = 0.2567
    bitcoind.rpc.sendtoaddress(addr, value_a)
    wait_for(lambda: len(minisafed.rpc.listcoins()["coins"]) == 1)
    outpoints = [c["outpoint"] for c in minisafed.rpc.listcoins()["coins"]]
    destinations = {
        bitcoind.rpc.getnewaddress(): int(value_a * COIN // 2),
    }
    res = minisafed.rpc.createspend(outpoints, destinations, 6)
    assert "psbt" in res

    addr = minisafed.rpc.getnewaddress()["address"]
    value_b = 0.0987
    bitcoind.rpc.sendtoaddress(addr, value_b)
    wait_for(lambda: len(minisafed.rpc.listcoins()["coins"]) == 2)
    outpoints = [c["outpoint"] for c in minisafed.rpc.listcoins()["coins"]]
    destinations = {
        bitcoind.rpc.getnewaddress(): int((value_a + value_b) * COIN - 1_000),
    }
    res_b = minisafed.rpc.createspend(outpoints, destinations, 2)
    assert "psbt" in res_b

    # Store them both in DB.
    assert len(minisafed.rpc.listspendtxs()["spend_txs"]) == 0
    minisafed.rpc.updatespend(res["psbt"])
    minisafed.rpc.updatespend(res_b["psbt"])

    # Listing all Spend transactions will list them both. It'll tell us which one has
    # change and which one doesn't.
    list_res = minisafed.rpc.listspendtxs()["spend_txs"]
    assert len(list_res) == 2
    first_psbt = next(entry for entry in list_res if entry["psbt"] == res["psbt"])
    assert first_psbt["change_index"] == 1
    second_psbt = next(entry for entry in list_res if entry["psbt"] == res_b["psbt"])
    assert second_psbt["change_index"] is None

    # If we delete the first one, we'll get only the second one.
    first_psbt = PSBT()
    first_psbt.deserialize(res["psbt"])
    minisafed.rpc.delspendtx(first_psbt.tx.txid().hex())
    list_res = minisafed.rpc.listspendtxs()["spend_txs"]
    assert len(list_res) == 1
    assert list_res[0]["psbt"] == res_b["psbt"]

    # If we delete the second one, result will be empty.
    second_psbt = PSBT()
    second_psbt.deserialize(res_b["psbt"])
    minisafed.rpc.delspendtx(second_psbt.tx.txid().hex())
    list_res = minisafed.rpc.listspendtxs()["spend_txs"]
    assert len(list_res) == 0


def test_update_spend(minisafed, bitcoind):
    # Start by creating a Spend PSBT
    addr = minisafed.rpc.getnewaddress()["address"]
    bitcoind.rpc.sendtoaddress(addr, 0.2567)
    wait_for(lambda: len(minisafed.rpc.listcoins()["coins"]) > 0)
    outpoints = [c["outpoint"] for c in minisafed.rpc.listcoins()["coins"]]
    destinations = {
        bitcoind.rpc.getnewaddress(): 200_000,
    }
    res = minisafed.rpc.createspend(outpoints, destinations, 6)
    assert "psbt" in res

    # Now update it
    assert len(minisafed.rpc.listspendtxs()["spend_txs"]) == 0
    minisafed.rpc.updatespend(res["psbt"])
    list_res = minisafed.rpc.listspendtxs()["spend_txs"]
    assert len(list_res) == 1
    assert list_res[0]["psbt"] == res["psbt"]

    # Keep a copy for later.
    psbt_no_sig = PSBT()
    psbt_no_sig.deserialize(res["psbt"])

    # We can add a signature and update it
    psbt_sig_a = PSBT()
    psbt_sig_a.deserialize(res["psbt"])
    dummy_pk_a = bytes.fromhex(
        "0375e00eb72e29da82b89367947f29ef34afb75e8654f6ea368e0acdfd92976b7c"
    )
    dummy_sig_a = bytes.fromhex(
        "304402202b925395cfeaa0171a7a92982bb4891acc4a312cbe7691d8375d36796d5b570a0220378a8ab42832848e15d1aedded5fb360fedbdd6c39226144e527f0f1e19d5398"
    )
    psbt_sig_a.inputs[0].partial_sigs[dummy_pk_a] = dummy_sig_a
    psbt_sig_a_ser = psbt_sig_a.serialize()
    minisafed.rpc.updatespend(psbt_sig_a_ser)

    # We'll get it when querying
    list_res = minisafed.rpc.listspendtxs()["spend_txs"]
    assert len(list_res) == 1
    assert list_res[0]["psbt"] == psbt_sig_a_ser

    # We can add another signature to the empty PSBT and update it again
    psbt_sig_b = PSBT()
    psbt_sig_b.deserialize(res["psbt"])
    dummy_pk_b = bytes.fromhex(
        "03a1b26313f430c4b15bb1fdce663207659d8cac749a0e53d70eff01874496feff"
    )
    dummy_sig_b = bytes.fromhex(
        "3044022005aebcd649fb8965f0591710fb3704931c3e8118ee60dd44917479f63ceba6d4022018b212900e5a80e9452366894de37f0d02fb9c89f1e94f34fb6ed7fd71c15c41"
    )
    psbt_sig_b.inputs[0].partial_sigs[dummy_pk_b] = dummy_sig_b
    psbt_sig_b_ser = psbt_sig_b.serialize()
    minisafed.rpc.updatespend(psbt_sig_b_ser)

    # It will have merged both.
    list_res = minisafed.rpc.listspendtxs()["spend_txs"]
    assert len(list_res) == 1
    psbt_merged = PSBT()
    psbt_merged.deserialize(list_res[0]["psbt"])
    assert len(psbt_merged.inputs[0].partial_sigs) == 2
    assert psbt_merged.inputs[0].partial_sigs[dummy_pk_a] == dummy_sig_a
    assert psbt_merged.inputs[0].partial_sigs[dummy_pk_b] == dummy_sig_b
