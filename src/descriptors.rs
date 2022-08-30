use miniscript::{
    descriptor,
    miniscript::{
        decode::Terminal,
        iter::PkPkh,
        limits::{SEQUENCE_LOCKTIME_DISABLE_FLAG, SEQUENCE_LOCKTIME_TYPE_FLAG},
        Miniscript,
    },
    policy::{Liftable, Semantic as SemanticPolicy},
    ScriptContext,
};

use std::{error, fmt, str, sync};

// Flag applied to the nSequence and CSV value before comparing them.
//
// <https://github.com/bitcoin/bitcoin/blob/4a540683ec40393d6369da1a9e02e45614db936d/src/primitives/transaction.h#L87-L89>
pub const SEQUENCE_LOCKTIME_MASK: u32 = 0x00_00_ff_ff;

#[derive(Debug)]
pub enum DescCreationError {
    InsaneTimelock(u32),
    InvalidKey(descriptor::DescriptorPublicKey),
    Miniscript(miniscript::Error),
    IncompatibleDesc,
}

impl std::fmt::Display for DescCreationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::InsaneTimelock(tl) => write!(f, "Timelock value '{}' isn't safe to use", tl),
            Self::InvalidKey(key) => {
                write!(f, "Invalid key '{}'. Need a wildcard ('ranged') xpub", key)
            }
            Self::Miniscript(e) => write!(f, "Miniscript error: '{}'.", e),
            Self::IncompatibleDesc => write!(f, "Descriptor is not compatible."),
        }
    }
}

impl error::Error for DescCreationError {}

// We require the locktime to:
//  - not be disabled
//  - be in number of blocks
//  - be 'clean' / minimal, ie all bits without consensus meaning should be 0
fn csv_check(csv: u32) -> Result<(), DescCreationError> {
    if (csv & SEQUENCE_LOCKTIME_DISABLE_FLAG) == 0
        && (csv & SEQUENCE_LOCKTIME_TYPE_FLAG) == 0
        && (csv & SEQUENCE_LOCKTIME_MASK) == csv
    {
        Ok(())
    } else {
        Err(DescCreationError::InsaneTimelock(csv))
    }
}

fn is_unhardened_deriv(key: &descriptor::DescriptorPublicKey) -> bool {
    match *key {
        descriptor::DescriptorPublicKey::SinglePub(..) => false,
        descriptor::DescriptorPublicKey::XPub(ref xpub) => {
            xpub.wildcard == descriptor::Wildcard::Unhardened
        }
    }
}

/// A Miniscript descriptor with a main, unencombered, branch (the main owner of the coins)
/// and a timelocked branch (the heir).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InheritanceDescriptor(descriptor::Descriptor<descriptor::DescriptorPublicKey>);

impl fmt::Display for InheritanceDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl str::FromStr for InheritanceDescriptor {
    type Err = DescCreationError;

    fn from_str(s: &str) -> Result<InheritanceDescriptor, Self::Err> {
        let wsh_desc = descriptor::Wsh::<descriptor::DescriptorPublicKey>::from_str(s)
            .map_err(DescCreationError::Miniscript)?;
        let ms = match wsh_desc.as_inner() {
            descriptor::WshInner::Ms(ms) => ms,
            _ => return Err(DescCreationError::IncompatibleDesc),
        };
        let invalid_key = ms.iter_pk_pkh().find_map(|pk_pkh| {
            let pk = match pk_pkh {
                PkPkh::PlainPubkey(pk) => pk,
                PkPkh::HashedPubkey(pk) => pk,
            };
            if is_unhardened_deriv(&pk) {
                None
            } else {
                Some(pk)
            }
        });
        if let Some(key) = invalid_key {
            return Err(DescCreationError::InvalidKey(key));
        }

        // Semantic of the Miniscript must be either the owner now, or the heir after
        // a timelock.
        let policy = ms
            .lift()
            .expect("Lifting can't fail on a Miniscript")
            .normalized();
        let subs = match policy {
            SemanticPolicy::Threshold(1, subs) => Some(subs),
            _ => None,
        }
        .ok_or(DescCreationError::IncompatibleDesc)?;
        if subs.len() != 2 {
            return Err(DescCreationError::IncompatibleDesc);
        }

        // Owner branch
        subs.iter()
            .find(|s| matches!(s, SemanticPolicy::KeyHash(_)))
            .ok_or(DescCreationError::IncompatibleDesc)?;

        // Heir branch
        let heir_subs = subs
            .iter()
            .find_map(|s| match s {
                SemanticPolicy::Threshold(2, subs) => Some(subs),
                _ => None,
            })
            .ok_or(DescCreationError::IncompatibleDesc)?;
        if heir_subs.len() != 2 {
            return Err(DescCreationError::IncompatibleDesc);
        }
        // Must be timelocked
        let csv = heir_subs
            .iter()
            .find_map(|s| match s {
                SemanticPolicy::Older(csv) => Some(csv),
                _ => None,
            })
            .ok_or(DescCreationError::IncompatibleDesc)?;
        csv_check(*csv)?;
        // And key locked
        heir_subs
            .iter()
            .find(|s| matches!(s, SemanticPolicy::KeyHash(_)))
            .ok_or(DescCreationError::IncompatibleDesc)?;

        Ok(InheritanceDescriptor(descriptor::Descriptor::Wsh(wsh_desc)))
    }
}

impl InheritanceDescriptor {
    pub fn new(
        owner_key: descriptor::DescriptorPublicKey,
        heir_key: descriptor::DescriptorPublicKey,
        timelock: u32,
    ) -> Result<descriptor::Descriptor<descriptor::DescriptorPublicKey>, DescCreationError> {
        csv_check(timelock)?;

        if let Some(key) = vec![&owner_key, &heir_key]
            .iter()
            .find(|k| !is_unhardened_deriv(k))
        {
            return Err(DescCreationError::InvalidKey((**key).clone()));
        }

        let owner_pk = Miniscript::from_ast(Terminal::Check(sync::Arc::from(
            Miniscript::from_ast(Terminal::PkK(owner_key)).expect("TODO"),
        )))
        .expect("Well typed");

        let heir_pkh = Miniscript::from_ast(Terminal::Check(sync::Arc::from(
            Miniscript::from_ast(Terminal::PkH(heir_key)).expect("TODO"),
        )))
        .expect("Well typed");

        let heir_timelock = Terminal::Older(timelock);
        let heir_branch = Miniscript::from_ast(Terminal::AndV(
            Miniscript::from_ast(Terminal::Verify(heir_pkh.into()))
                .expect("Well typed")
                .into(),
            Miniscript::from_ast(heir_timelock)
                .expect("Well typed")
                .into(),
        ))
        .expect("Well typed");

        let tl_miniscript =
            Miniscript::from_ast(Terminal::OrD(owner_pk.into(), heir_branch.into()))
                .expect("Well typed");
        miniscript::Segwitv0::check_local_validity(&tl_miniscript)
            .expect("Miniscript must be sane");

        Ok(descriptor::Descriptor::Wsh(
            descriptor::Wsh::new(tl_miniscript).expect("Must pass sanity checks"),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::str::FromStr;

    #[test]
    fn inheritance_descriptor_creation() {
        let owner_key = descriptor::DescriptorPublicKey::from_str(&"xpub6Eze7yAT3Y1wGrnzedCNVYDXUqa9NmHVWck5emBaTbXtURbe1NWZbK9bsz1TiVE7Cz341PMTfYgFw1KdLWdzcM1UMFTcdQfCYhhXZ2HJvTW/*").unwrap();
        let heir_key = descriptor::DescriptorPublicKey::from_str(&"xpub688Hn4wScQAAiYJLPg9yH27hUpfZAUnmJejRQBCiwfP5PEDzjWMNW1wChcninxr5gyavFqbbDjdV1aK5USJz8NDVjUy7FRQaaqqXHh5SbXe/*").unwrap();
        let timelock = 52560;
        assert_eq!(InheritanceDescriptor::new(owner_key.clone(), heir_key.clone(), timelock).unwrap().to_string(), "wsh(or_d(pk(xpub6Eze7yAT3Y1wGrnzedCNVYDXUqa9NmHVWck5emBaTbXtURbe1NWZbK9bsz1TiVE7Cz341PMTfYgFw1KdLWdzcM1UMFTcdQfCYhhXZ2HJvTW/*),and_v(v:pkh(xpub688Hn4wScQAAiYJLPg9yH27hUpfZAUnmJejRQBCiwfP5PEDzjWMNW1wChcninxr5gyavFqbbDjdV1aK5USJz8NDVjUy7FRQaaqqXHh5SbXe/*),older(52560))))#eeyujkt7");

        // We prevent footguns with timelocks
        InheritanceDescriptor::new(owner_key.clone(), heir_key.clone(), 0x00_01_0f_00).unwrap_err();
        InheritanceDescriptor::new(owner_key.clone(), heir_key.clone(), (1 << 31) + 1).unwrap_err();
        InheritanceDescriptor::new(owner_key.clone(), heir_key.clone(), (1 << 22) + 1).unwrap_err();

        let owner_key = descriptor::DescriptorPublicKey::from_str(&"[aabb0011/10/4893]xpub661MyMwAqRbcFG59fiikD8UV762quhruT8K8bdjqy6N2o3LG7yohoCdLg1m2HAY1W6rfBrtauHkBhbfA4AQ3iazaJj5wVPhwgaRCHBW2DBg/*").unwrap();
        let heir_key = descriptor::DescriptorPublicKey::from_str(&"xpub661MyMwAqRbcFfxf71L4Dx4w5TmyNXrBicTEAM7vLzumxangwATWWgdJPb6xH1JHcJH9S3jNZx3fCnkkB1WyqrqGgavj1rehHcbythmruvZ/24/32/*").unwrap();
        let timelock = 57600;
        assert_eq!(InheritanceDescriptor::new(owner_key.clone(), heir_key, timelock).unwrap().to_string(), "wsh(or_d(pk([aabb0011/10/4893]xpub661MyMwAqRbcFG59fiikD8UV762quhruT8K8bdjqy6N2o3LG7yohoCdLg1m2HAY1W6rfBrtauHkBhbfA4AQ3iazaJj5wVPhwgaRCHBW2DBg/*),and_v(v:pkh(xpub661MyMwAqRbcFfxf71L4Dx4w5TmyNXrBicTEAM7vLzumxangwATWWgdJPb6xH1JHcJH9S3jNZx3fCnkkB1WyqrqGgavj1rehHcbythmruvZ/24/32/*),older(57600))))#8kamh6y8");

        // We can't pass a raw key, an xpub that is not deriveable, or only hardened derivable
        let heir_key = descriptor::DescriptorPublicKey::from_str(&"xpub661MyMwAqRbcFfxf71L4Dx4w5TmyNXrBicTEAM7vLzumxangwATWWgdJPb6xH1JHcJH9S3jNZx3fCnkkB1WyqrqGgavj1rehHcbythmruvZ/0/354").unwrap();
        InheritanceDescriptor::new(owner_key.clone(), heir_key, timelock).unwrap_err();
        let heir_key = descriptor::DescriptorPublicKey::from_str(&"xpub661MyMwAqRbcFfxf71L4Dx4w5TmyNXrBicTEAM7vLzumxangwATWWgdJPb6xH1JHcJH9S3jNZx3fCnkkB1WyqrqGgavj1rehHcbythmruvZ/0/*'").unwrap();
        InheritanceDescriptor::new(owner_key.clone(), heir_key, timelock).unwrap_err();
        let heir_key = descriptor::DescriptorPublicKey::from_str(
            &"02e24913be26dbcfdf8e8e94870b28725cdae09b448b6c127767bf0154e3a3c8e5",
        )
        .unwrap();
        InheritanceDescriptor::new(owner_key.clone(), heir_key, timelock).unwrap_err();
    }
}
