//! structs from the esplora API
//!
//! see: <https://github.com/Blockstream/esplora/blob/master/API.md>

use std::future::Future;
use std::pin::Pin;

pub use bitcoin::consensus::{deserialize, serialize};
pub use bitcoin::hex::FromHex;
use bitcoin::Weight;
pub use bitcoin::{
    transaction, Amount, BlockHash, OutPoint, ScriptBuf, Transaction, TxIn, TxOut, Txid, Witness,
};

use hex::DisplayHex;
use serde::Deserialize;

pub enum ParseResponseError {
    Json(serde_json::Error),
    UnhandledStatus(i32),
}

#[derive(Debug)]
pub enum SendError<E> {
    Client(E),
    UnhandledStatus(i32, Vec<u8>),
    Parse(i32, serde_json::Error),
}

pub type Request = (&'static str, String, Vec<u8>);

type FutureResult<'a, T, E> = Pin<Box<dyn Future<Output = Result<T, E>> + Send + 'a>>;

pub trait ClientRequest: Sized {
    type Output: for<'a> serde::de::Deserialize<'a>;

    fn request(&self) -> Request;

    fn parse_response(&self, status: i32, body: &[u8]) -> Result<Self::Output, ParseResponseError>;

    fn send<F, E>(&self, handle: &mut F) -> Result<Self::Output, SendError<E>>
    where
        F: FnMut(&'static str, String, Vec<u8>) -> Result<(i32, Vec<u8>), E>,
    {
        let (req_method, req_path, req_body) = self.request();
        let (resp_status, resp_body) =
            handle(req_method, req_path, req_body).map_err(SendError::Client)?;
        self.parse_response(resp_status, resp_body.as_ref())
            .map_err(|err| match err {
                ParseResponseError::Json(err) => SendError::Parse(resp_status, err),
                ParseResponseError::UnhandledStatus(status) => {
                    SendError::UnhandledStatus(status, resp_body)
                }
            })
    }

    fn send_async<'a, F, E>(
        &'a self,
        handle: &'a mut F,
    ) -> FutureResult<'a, Self::Output, SendError<E>>
    where
        F: FnMut(&'static str, String, Vec<u8>) -> FutureResult<'a, (i32, Vec<u8>), E>
            + Send
            + Sync,
        Self: Sync,
    {
        Box::pin(async move {
            let (req_method, req_path, req_body) = self.request();
            let (resp_status, resp_body) = handle(req_method, req_path, req_body)
                .await
                .map_err(SendError::Client)?;
            self.parse_response(resp_status, resp_body.as_ref())
                .map_err(|err| match err {
                    ParseResponseError::Json(err) => SendError::Parse(resp_status, err),
                    ParseResponseError::UnhandledStatus(status) => {
                        SendError::UnhandledStatus(status, resp_body)
                    }
                })
        })
    }
}

pub trait ClientRequestSender {
    type Error;
}

pub struct GetTx {
    pub txid: Txid,
}

impl GetTx {
    pub fn new(txid: Txid) -> Self {
        Self { txid }
    }
}

impl ClientRequest for GetTx {
    type Output = Option<Transaction>;

    fn request(&self) -> Request {
        (
            "GET",
            format!("/tx/{}/raw", self.txid),
            Vec::with_capacity(0),
        )
    }

    fn parse_response(&self, status: i32, body: &[u8]) -> Result<Self::Output, ParseResponseError> {
        match status {
            200 => Ok(serde_json::from_slice(body).map_err(ParseResponseError::Json)?),
            404 => Ok(None),
            error_status => Err(ParseResponseError::UnhandledStatus(error_status)),
        }
    }
}

pub struct PostTx {
    pub tx: Transaction,
}

impl ClientRequest for PostTx {
    type Output = ();

    fn request(&self) -> Request {
        (
            "POST",
            "/tx".to_string(),
            bitcoin::consensus::encode::serialize(&self.tx)
                .to_lower_hex_string()
                .as_bytes()
                .to_vec(),
        )
    }

    fn parse_response(
        &self,
        status: i32,
        _body: &[u8],
    ) -> Result<Self::Output, ParseResponseError> {
        match status {
            200 => Ok(()),
            error_status => Err(ParseResponseError::UnhandledStatus(error_status)),
        }
    }
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct PrevOut {
    pub value: u64,
    pub scriptpubkey: ScriptBuf,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Vin {
    pub txid: Txid,
    pub vout: u32,
    // None if coinbase
    pub prevout: Option<PrevOut>,
    pub scriptsig: ScriptBuf,
    #[serde(deserialize_with = "deserialize_witness", default)]
    pub witness: Vec<Vec<u8>>,
    pub sequence: u32,
    pub is_coinbase: bool,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Vout {
    pub value: u64,
    pub scriptpubkey: ScriptBuf,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct TxStatus {
    pub confirmed: bool,
    pub block_height: Option<u32>,
    pub block_hash: Option<BlockHash>,
    pub block_time: Option<u64>,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct MerkleProof {
    pub block_height: u32,
    pub merkle: Vec<Txid>,
    pub pos: usize,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OutputStatus {
    pub spent: bool,
    pub txid: Option<Txid>,
    pub vin: Option<u64>,
    pub status: Option<TxStatus>,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BlockStatus {
    pub in_best_chain: bool,
    pub height: Option<u32>,
    pub next_best: Option<BlockHash>,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Tx {
    pub txid: Txid,
    pub version: i32,
    pub locktime: u32,
    pub vin: Vec<Vin>,
    pub vout: Vec<Vout>,
    /// Transaction size in raw bytes (NOT virtual bytes).
    pub size: usize,
    /// Transaction weight units.
    pub weight: u64,
    pub status: TxStatus,
    pub fee: u64,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BlockTime {
    pub timestamp: u64,
    pub height: u32,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct BlockSummary {
    pub id: BlockHash,
    #[serde(flatten)]
    pub time: BlockTime,
    /// Hash of the previous block, will be `None` for the genesis block.
    pub previousblockhash: Option<bitcoin::BlockHash>,
    pub merkle_root: bitcoin::hash_types::TxMerkleNode,
}

impl Tx {
    pub fn to_tx(&self) -> Transaction {
        Transaction {
            version: transaction::Version::non_standard(self.version),
            lock_time: bitcoin::absolute::LockTime::from_consensus(self.locktime),
            input: self
                .vin
                .iter()
                .cloned()
                .map(|vin| TxIn {
                    previous_output: OutPoint {
                        txid: vin.txid,
                        vout: vin.vout,
                    },
                    script_sig: vin.scriptsig,
                    sequence: bitcoin::Sequence(vin.sequence),
                    witness: Witness::from_slice(&vin.witness),
                })
                .collect(),
            output: self
                .vout
                .iter()
                .cloned()
                .map(|vout| TxOut {
                    value: Amount::from_sat(vout.value),
                    script_pubkey: vout.scriptpubkey,
                })
                .collect(),
        }
    }

    pub fn confirmation_time(&self) -> Option<BlockTime> {
        match self.status {
            TxStatus {
                confirmed: true,
                block_height: Some(height),
                block_time: Some(timestamp),
                ..
            } => Some(BlockTime { timestamp, height }),
            _ => None,
        }
    }

    pub fn previous_outputs(&self) -> Vec<Option<TxOut>> {
        self.vin
            .iter()
            .cloned()
            .map(|vin| {
                vin.prevout.map(|po| TxOut {
                    script_pubkey: po.scriptpubkey,
                    value: Amount::from_sat(po.value),
                })
            })
            .collect()
    }

    pub fn weight(&self) -> Weight {
        Weight::from_wu(self.weight)
    }

    pub fn fee(&self) -> Amount {
        Amount::from_sat(self.fee)
    }
}

fn deserialize_witness<'de, D>(d: D) -> Result<Vec<Vec<u8>>, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    let list = Vec::<String>::deserialize(d)?;
    list.into_iter()
        .map(|hex_str| Vec::<u8>::from_hex(&hex_str))
        .collect::<Result<Vec<Vec<u8>>, _>>()
        .map_err(serde::de::Error::custom)
}
