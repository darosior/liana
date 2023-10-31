from fixtures import *
from test_framework.serializations import PSBT
from test_framework.utils import wait_for, COIN, RpcError


def test_spend_change(lianad, bitcoind):
    """We can spend a coin that was received on a change address."""
    # Receive a coin on a receive address
    addr = lianad.rpc.getnewaddress()["address"]
    txid = bitcoind.rpc.sendtoaddress(addr, 0.01)
    bitcoind.generate_block(1, wait_for_mempool=txid)
    wait_for(lambda: len(lianad.rpc.listcoins()["coins"]) == 1)

    # Create a transaction that will spend this coin to 1) one of our receive
    # addresses 2) an external address 3) one of our change addresses.
    outpoints = [c["outpoint"] for c in lianad.rpc.listcoins()["coins"]]
    destinations = {
        bitcoind.rpc.getnewaddress(): 100_000,
        lianad.rpc.getnewaddress()["address"]: 100_000,
    }
    res = lianad.rpc.createspend(destinations, outpoints, 2)
    assert "psbt" in res

    # The transaction must contain a change output.
    spend_psbt = PSBT.from_base64(res["psbt"])
    assert len(spend_psbt.o) == 3
    assert len(spend_psbt.tx.vout) == 3

    # Sign and broadcast this first Spend transaction.
    signed_psbt = lianad.signer.sign_psbt(spend_psbt)
    lianad.rpc.updatespend(signed_psbt.to_base64())
    spend_txid = signed_psbt.tx.txid().hex()
    lianad.rpc.broadcastspend(spend_txid)
    bitcoind.generate_block(1, wait_for_mempool=spend_txid)
    wait_for(lambda: len(lianad.rpc.listcoins()["coins"]) == 3)

    # Now create a new transaction that spends the change output as well as
    # the output sent to the receive address.
    outpoints = [
        c["outpoint"]
        for c in lianad.rpc.listcoins()["coins"]
        if c["spend_info"] is None
    ]
    destinations = {
        bitcoind.rpc.getnewaddress(): 100_000,
    }
    res = lianad.rpc.createspend(destinations, outpoints, 2)
    spend_psbt = PSBT.from_base64(res["psbt"])

    # We can sign and broadcast it.
    signed_psbt = lianad.signer.sign_psbt(spend_psbt)
    lianad.rpc.updatespend(signed_psbt.to_base64())
    spend_txid = signed_psbt.tx.txid().hex()
    lianad.rpc.broadcastspend(spend_txid)
    bitcoind.generate_block(1, wait_for_mempool=spend_txid)


def test_coin_marked_spent(lianad, bitcoind):
    """Test a spent coin is marked as such under various conditions."""
    # Receive a coin in a single transaction
    addr = lianad.rpc.getnewaddress()["address"]
    deposit_a = bitcoind.rpc.sendtoaddress(addr, 0.01)
    bitcoind.generate_block(1, wait_for_mempool=deposit_a)
    wait_for(lambda: len(lianad.rpc.listcoins()["coins"]) == 1)

    # Receive another coin on the same address
    deposit_b = bitcoind.rpc.sendtoaddress(addr, 0.02)
    bitcoind.generate_block(1, wait_for_mempool=deposit_b)
    wait_for(lambda: len(lianad.rpc.listcoins()["coins"]) == 2)

    # Receive three coins in a single deposit transaction
    destinations = {
        lianad.rpc.getnewaddress()["address"]: 0.03,
        lianad.rpc.getnewaddress()["address"]: 0.04,
        lianad.rpc.getnewaddress()["address"]: 0.05,
    }
    deposit_c = bitcoind.rpc.sendmany("", destinations)
    bitcoind.generate_block(1, wait_for_mempool=deposit_c)
    wait_for(lambda: len(lianad.rpc.listcoins()["coins"]) == 5)

    # Receive a coin in an unconfirmed deposit transaction
    addr = lianad.rpc.getnewaddress()["address"]
    deposit_d = bitcoind.rpc.sendtoaddress(addr, 0.06)
    wait_for(lambda: len(lianad.rpc.listcoins()["coins"]) == 5)

    def sign_and_broadcast(psbt):
        txid = psbt.tx.txid().hex()
        psbt = lianad.signer.sign_psbt(psbt)
        lianad.rpc.updatespend(psbt.to_base64())
        lianad.rpc.broadcastspend(txid)
        return txid

    # Spend the first coin with a change output
    outpoint = next(
        c["outpoint"]
        for c in lianad.rpc.listcoins()["coins"]
        if deposit_a in c["outpoint"]
    )
    destinations = {
        bitcoind.rpc.getnewaddress(): 500_000,
    }
    res = lianad.rpc.createspend(destinations, [outpoint], 6)
    psbt = PSBT.from_base64(res["psbt"])
    sign_and_broadcast(psbt)

    # Spend the second coin without a change output
    outpoint = next(
        c["outpoint"]
        for c in lianad.rpc.listcoins()["coins"]
        if deposit_b in c["outpoint"]
    )
    destinations = {
        bitcoind.rpc.getnewaddress(): int(0.02 * COIN) - 1_000,
    }
    res = lianad.rpc.createspend(destinations, [outpoint], 1)
    psbt = PSBT.from_base64(res["psbt"])
    sign_and_broadcast(psbt)

    # Spend the third coin to an address of ours, no change
    outpoints = [
        c["outpoint"]
        for c in lianad.rpc.listcoins()["coins"]
        if deposit_c in c["outpoint"]
    ]
    destinations = {
        lianad.rpc.getnewaddress()["address"]: int(0.03 * COIN) - 1_000,
    }
    res = lianad.rpc.createspend(destinations, [outpoints[0]], 1)
    psbt = PSBT.from_base64(res["psbt"])
    sign_and_broadcast(psbt)

    # Spend the fourth coin to an address of ours, with change
    destinations = {
        lianad.rpc.getnewaddress()["address"]: int(0.04 * COIN / 2),
    }
    res = lianad.rpc.createspend(destinations, [outpoints[1]], 18)
    psbt = PSBT.from_base64(res["psbt"])
    sign_and_broadcast(psbt)

    # Batch spend the fourth and fifth coins
    outpoint = next(
        c["outpoint"]
        for c in lianad.rpc.listcoins()["coins"]
        if deposit_d in c["outpoint"]
    )
    destinations = {
        lianad.rpc.getnewaddress()["address"]: int(0.01 * COIN),
        lianad.rpc.getnewaddress()["address"]: int(0.01 * COIN),
        bitcoind.rpc.getnewaddress(): int(0.01 * COIN),
    }
    res = lianad.rpc.createspend(destinations, [outpoints[2], outpoint], 2)
    psbt = PSBT.from_base64(res["psbt"])
    sign_and_broadcast(psbt)

    # All the spent coins must have been detected as such
    all_deposits = (deposit_a, deposit_b, deposit_c, deposit_d)

    def deposited_coins():
        return (
            c
            for c in lianad.rpc.listcoins()["coins"]
            if c["outpoint"][:-2] in all_deposits
        )

    def is_spent(coin):
        if coin["spend_info"] is None:
            return False
        if coin["spend_info"]["txid"] is None:
            return False
        return True

    wait_for(lambda: all(is_spent(c) for c in deposited_coins()))


def test_send_to_self(lianad, bitcoind):
    """Test we can use createspend with no destination to send to a change address."""
    # Get 3 coins.
    destinations = {
        lianad.rpc.getnewaddress()["address"]: 0.03,
        lianad.rpc.getnewaddress()["address"]: 0.04,
        lianad.rpc.getnewaddress()["address"]: 0.05,
    }
    deposit_txid = bitcoind.rpc.sendmany("", destinations)
    bitcoind.generate_block(1, wait_for_mempool=deposit_txid)
    wait_for(lambda: len(lianad.rpc.listcoins()["coins"]) == 3)

    # Then create a send-to-self transaction (by not providing any destination) that
    # sweeps them all.
    outpoints = [c["outpoint"] for c in lianad.rpc.listcoins()["coins"]]
    specified_feerate = 142
    res = lianad.rpc.createspend({}, outpoints, specified_feerate)
    spend_psbt = PSBT.from_base64(res["psbt"])
    assert len(spend_psbt.o) == len(spend_psbt.tx.vout) == 1

    # Note they may ask for an impossible send-to-self. In this case we'll error cleanly.
    with pytest.raises(
        RpcError,
        match="Not enough fund to create a 40500 sat/vb transaction with input value 0.12 BTC",
    ):
        lianad.rpc.createspend({}, outpoints, 40500)

    # Sign and broadcast the send-to-self transaction created above.
    signed_psbt = lianad.signer.sign_psbt(spend_psbt)
    lianad.rpc.updatespend(signed_psbt.to_base64())
    spend_txid = signed_psbt.tx.txid().hex()
    lianad.rpc.broadcastspend(spend_txid)

    # The only output is the change output so the feerate of the transaction must
    # not be lower than the one provided, and only possibly slightly higher (since
    # we slightly overestimate the satisfaction size).
    # FIXME: a 15% increase is huge.
    res = bitcoind.rpc.getmempoolentry(spend_txid)
    spend_feerate = int(res["fees"]["base"] * COIN / res["vsize"])
    assert specified_feerate <= spend_feerate <= int(specified_feerate * 115 / 100)

    # We should by now only have one coin.
    bitcoind.generate_block(1, wait_for_mempool=spend_txid)
    unspent_coins = lambda: (
        c for c in lianad.rpc.listcoins()["coins"] if c["spend_info"] is None
    )
    wait_for(lambda: len(list(unspent_coins())) == 1)


def test_coin_selection(lianad, bitcoind):
    """We can create a spend using coin selection."""
    # Do not specify any coins in outpoints list in order to use coin selection.
    outpoints = []
    # Send to an (external) address.
    dest_100_000 = {bitcoind.rpc.getnewaddress(): 100_000}
    # Coin selection is not possible if we have no coins.
    assert len(lianad.rpc.listcoins()["coins"]) == 0
    with pytest.raises(
        RpcError,
        match="Coin selection error: 'Insufficient funds. Missing \\d+ sats.'",
    ):
        lianad.rpc.createspend(dest_100_000, outpoints, 2)

    # Receive a coin in an unconfirmed deposit transaction.
    recv_addr = lianad.rpc.getnewaddress()["address"]
    deposit = bitcoind.rpc.sendtoaddress(recv_addr, 0.0008)  # 80_000 sats
    wait_for(lambda: len(lianad.rpc.listcoins()["coins"]) == 1)
    assert (
        len(
            [
                c
                for c in lianad.rpc.listcoins()["coins"]
                if c["block_height"] is not None
            ]
        )
        == 0
    )
    # There are still no coin selection candidates.
    with pytest.raises(
        RpcError,
        match="Coin selection error: 'Insufficient funds. Missing \\d+ sats.'",
    ):
        lianad.rpc.createspend(dest_100_000, outpoints, 2)

    # Confirm coin.
    bitcoind.generate_block(1, wait_for_mempool=deposit)
    confirmed_coins = lambda: (
        c for c in lianad.rpc.listcoins()["coins"] if c["block_height"] is not None
    )
    wait_for(lambda: len(list(confirmed_coins())) == 1)

    # Insufficient funds for coin selection.
    with pytest.raises(
        RpcError,
        match="Coin selection error: 'Insufficient funds. Missing \\d+ sats.'",
    ):
        lianad.rpc.createspend(dest_100_000, outpoints, 2)

    # Reduce spend amount.
    dest_30_000 = {bitcoind.rpc.getnewaddress(): 30_000}
    res = lianad.rpc.createspend(dest_30_000, outpoints, 2)
    assert "psbt" in res

    # The transaction must contain a change output.
    spend_psbt = PSBT.from_base64(res["psbt"])
    assert len(spend_psbt.o) == 2
    assert len(spend_psbt.tx.vout) == 2

    # Sign and broadcast this Spend transaction.
    signed_psbt = lianad.signer.sign_psbt(spend_psbt)
    lianad.rpc.updatespend(signed_psbt.to_base64())
    spend_txid = signed_psbt.tx.txid().hex()
    lianad.rpc.broadcastspend(spend_txid)

    wait_for(lambda: len(lianad.rpc.listcoins()["coins"]) == 2)
    coins = lianad.rpc.listcoins()["coins"]
    # Check that change output is unconfirmed.
    assert (
        len(
            [
                c
                for c in coins
                if c["spend_info"] is None and c["block_height"] is not None
            ]
        )
        == 0
    )
    # Check we cannot use coins as candidates if they have been spent or are unconfirmed.
    with pytest.raises(
        RpcError,
        match="Coin selection error: 'Insufficient funds. Missing \\d+ sats.'",
    ):
        lianad.rpc.createspend(dest_30_000, outpoints, 2)

    # Now confirm the Spend.
    bitcoind.generate_block(1, wait_for_mempool=spend_txid)
    wait_for(lambda: len(list(confirmed_coins())) == 2)

    # We now have an unspent coin to use as candidate for coin selection.
    assert (
        len([c for c in lianad.rpc.listcoins()["coins"] if c["spend_info"] is None])
        == 1
    )
    # But its value is not enough for this Spend.
    dest_60_000 = {bitcoind.rpc.getnewaddress(): 60_000}
    with pytest.raises(
        RpcError,
        match="Coin selection error: 'Insufficient funds. Missing \\d+ sats.'",
    ):
        lianad.rpc.createspend(dest_60_000, outpoints, 2)

    # Get another coin to check coin selection with more than one candidate.
    recv_addr = lianad.rpc.getnewaddress()["address"]
    deposit = bitcoind.rpc.sendtoaddress(recv_addr, 0.0002)  # 20_000 sats
    bitcoind.generate_block(1, wait_for_mempool=deposit)
    wait_for(lambda: len(list(confirmed_coins())) == 3)

    res = lianad.rpc.createspend(dest_60_000, outpoints, 2)
    assert "psbt" in res

    # The transaction must contain a change output.
    spend_psbt = PSBT.from_base64(res["psbt"])
    assert len(spend_psbt.o) == 2
    assert len(spend_psbt.tx.vout) == 2

    # Sign and broadcast this Spend transaction.
    signed_psbt = lianad.signer.sign_psbt(spend_psbt)
    lianad.rpc.updatespend(signed_psbt.to_base64())
    spend_txid = signed_psbt.tx.txid().hex()
    lianad.rpc.broadcastspend(spend_txid)
    bitcoind.generate_block(1, wait_for_mempool=spend_txid)
    wait_for(lambda: len(list(confirmed_coins())) == 4)
