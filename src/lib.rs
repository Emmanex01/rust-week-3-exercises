use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Deref;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct CompactSize {
    pub value: u64,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BitcoinError {
    InsufficientBytes,
    InvalidFormat,
}

impl CompactSize {
    pub fn new(value: u64) -> Self {
        // TODO: Construct a CompactSize from a u64 value
        CompactSize { value }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        // TODO: Encode according to Bitcoin's CompactSize format:

        // [0x00–0xFC] => 1 byte
        if self.value <= 0xFC {
            vec![self.value as u8]
        } else if self.value <= u16::MAX as u64 {
            // [0xFDxxxx] => 0xFD + u16 (2 bytes)
            // create a vec array using the 0xFD byte
            let mut bytes = vec![0xFD];

            // cast self.value a u64 to a byte and convert to a byte. the expression below returns a vec of bytes
            let bytes_value = (self.value as u16).to_le_bytes();

            // Append it to the bytes vec
            bytes.extend_from_slice(&bytes_value);

            bytes
        } else if self.value <= u32::MAX as u64 {
            // [0xFExxxxxxxx] => 0xFE + u32 (4 bytes)
            // create a vec array using the 0xFE byte
            let mut bytes = vec![0xFE];

            // cast self.value a u64 to a byte and convert to a byte. the expression below returns a vec of bytes
            let bytes_value = (self.value as u32).to_le_bytes();

            // Append it to the bytes vec
            bytes.extend_from_slice(&bytes_value);

            bytes
        } else {
            // [0xFFxxxxxxxxxxxxxxxx] => 0xFF + u64 (8 bytes)
            let mut bytes = vec![0xFF];
            // The value is already a u64, so no cast is needed.
            let value_bytes = self.value.to_le_bytes();
            // Append the 8 bytes to our vector.
            bytes.extend_from_slice(&value_bytes);
            bytes
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        // TODO: Decode CompactSize, returning value and number of bytes consumed.
        // First check if bytes is empty.
        if bytes.is_empty() {
            return Err(BitcoinError::InsufficientBytes);
        }
        // Check that enough bytes are available based on prefix.
        let prefix = bytes[0];

        match prefix {
            0x00..=0xfc => Ok((Self{value: prefix as u64}, 1)),
            0xfd => {
                // Check if we have at least 2 bytes for u16
                if bytes.len() < 3 {
                    return Err(BitcoinError::InsufficientBytes);
                }
                let value = u16::from_le_bytes(bytes[1..3].try_into().unwrap()) as u64;
                Ok((Self{value}, 3))
            }
            
            0xfe => {
                // Check if we have at least 4 bytes for u32
                if bytes.len() < 5 {
                    return Err(BitcoinError::InsufficientBytes);
                }
                let value = u32::from_le_bytes(bytes[1..5].try_into().unwrap()) as u64;
                Ok((Self{value}, 5))
            }

            0xff => {
                // Check if we have at least 8 bytes for u64
                if bytes.len() < 9 {
                    return Err(BitcoinError::InsufficientBytes);
                }
                let value = u64::from_le_bytes(bytes[1..9].try_into().unwrap());
                Ok((Self{value}, 9))
            }
        }
        
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Txid(pub [u8; 32]);

impl Serialize for Txid {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let hex_string = hex::encode(self.0);
        
        // TODO: Serialize as a hex-encoded string (32 bytes => 64 hex characters)
        serializer.serialize_str(&hex_string)
    }
}

impl<'de> Deserialize<'de> for Txid {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let hex_string: String = Deserialize::deserialize(deserializer)?;
        // TODO: Parse hex string into 32-byte array
        let bytes = hex::decode(&hex_string).map_err(serde::de::Error::custom)?;
        // Use `hex::decode`, validate length = 32
        if bytes.len() != 32 {
            return Err(serde::de::Error::custom("Invalid Txid length"));
        }
        let mut array = [0u8; 32];
        array.copy_from_slice(&bytes);
        Ok(Txid(array))
    }
}

// #[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
// pub struct OutPoint {
//     pub txid: Txid,
//     pub vout: u32,
// }

// impl OutPoint {
//     pub fn new(txid: [u8; 32], vout: u32) -> Self {
//         // TODO: Create an OutPoint from raw txid bytes and output index
//     }

//     pub fn to_bytes(&self) -> Vec<u8> {
//         // TODO: Serialize as: txid (32 bytes) + vout (4 bytes, little-endian)
//     }

//     pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
//         // TODO: Deserialize 36 bytes: txid[0..32], vout[32..36]
//         // Return error if insufficient bytes
//     }
// }

// #[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
// pub struct Script {
//     pub bytes: Vec<u8>,
// }

// impl Script {
//     pub fn new(bytes: Vec<u8>) -> Self {
//         // TODO: Simple constructor
//     }

//     pub fn to_bytes(&self) -> Vec<u8> {
//         // TODO: Prefix with CompactSize (length), then raw bytes
//     }

//     pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
//         // TODO: Parse CompactSize prefix, then read that many bytes
//         // Return error if not enough bytes
//     }
// }

// impl Deref for Script {
//     type Target = Vec<u8>;
//     fn deref(&self) -> &Self::Target {
//         // TODO: Allow &Script to be used as &[u8]
//     }
// }

// #[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
// pub struct TransactionInput {
//     pub previous_output: OutPoint,
//     pub script_sig: Script,
//     pub sequence: u32,
// }

// impl TransactionInput {
//     pub fn new(previous_output: OutPoint, script_sig: Script, sequence: u32) -> Self {
//         // TODO: Basic constructor
//     }

//     pub fn to_bytes(&self) -> Vec<u8> {
//         // TODO: Serialize: OutPoint + Script (with CompactSize) + sequence (4 bytes LE)
//     }

//     pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
//         // TODO: Deserialize in order:
//         // - OutPoint (36 bytes)
//         // - Script (with CompactSize)
//         // - Sequence (4 bytes)
//     }
// }

// #[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
// pub struct BitcoinTransaction {
//     pub version: u32,
//     pub inputs: Vec<TransactionInput>,
//     pub lock_time: u32,
// }

// impl BitcoinTransaction {
//     pub fn new(version: u32, inputs: Vec<TransactionInput>, lock_time: u32) -> Self {
//         // TODO: Construct a transaction from parts
//     }

//     pub fn to_bytes(&self) -> Vec<u8> {
//         // TODO: Format:
//         // - version (4 bytes LE)
//         // - CompactSize (number of inputs)
//         // - each input serialized
//         // - lock_time (4 bytes LE)
//     }

//     pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
//         // TODO: Read version, CompactSize for input count
//         // Parse inputs one by one
//         // Read final 4 bytes for lock_time
//     }
// }

// impl fmt::Display for BitcoinTransaction {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         // TODO: Format a user-friendly string showing version, inputs, lock_time
//         // Display scriptSig length and bytes, and previous output info
//     }
// }
