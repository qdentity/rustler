use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::hash::{Hash, Hasher};

#[derive(Clone)]
pub enum StringOrBin {
    String(String),
    Bin(Vec<u8>),
}

impl StringOrBin {

    pub fn from_bytes(bytes: &[u8]) -> Self {
        match std::str::from_utf8(bytes) {
            Ok(string) => StringOrBin::String(string.to_owned()),
            Err(_) => StringOrBin::Bin(bytes.to_owned()),
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        match self {
            StringOrBin::Bin(bin) => bin.as_slice(),
            StringOrBin::String(string) => string.as_bytes(),
        }
    }

}

impl Hash for StringOrBin {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.as_bytes().hash(h)
    }
}
impl PartialEq for StringOrBin {
    fn eq(&self, rhs: &StringOrBin) -> bool {
        self.as_bytes() == rhs.as_bytes()
    }
}
impl Eq for StringOrBin {
}

impl Debug for StringOrBin {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            StringOrBin::String(string) => write!(f, "{:?}", string),
            StringOrBin::Bin(data) => write!(f, "{:?}", data),
        }
    }
}

impl From<&str> for StringOrBin {
    fn from(string: &str) -> Self {
        string.to_owned().into()
    }
}
impl From<String> for StringOrBin {
    fn from(string: String) -> Self {
        StringOrBin::String(string)
    }
}
impl From<Vec<u8>> for StringOrBin {
    fn from(bin: Vec<u8>) -> Self {
        match String::from_utf8(bin) {
            Ok(string) => StringOrBin::String(string),
            Err(err) => StringOrBin::Bin(err.into_bytes()),
        }
    }
}
