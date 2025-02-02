use crate::daemon::DaemonError;
use minisafe::config::ConfigError;
use std::convert::From;
use std::io::ErrorKind;

#[derive(Debug, Clone)]
pub enum Error {
    Config(String),
    Daemon(DaemonError),
    Unexpected(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Config(e) => write!(f, "{}", e),
            Self::Daemon(e) => match e {
                DaemonError::Unexpected(e) => write!(f, "{}", e),
                DaemonError::NoAnswer => write!(f, "Daemon did not answer"),
                DaemonError::Transport(Some(ErrorKind::ConnectionRefused), _) => {
                    write!(f, "Failed to connect to daemon")
                }
                DaemonError::Transport(kind, e) => {
                    if let Some(k) = kind {
                        write!(f, "{} [{:?}]", e, k)
                    } else {
                        write!(f, "{}", e)
                    }
                }
                DaemonError::Start(e) => {
                    write!(f, "Failed to start daemon: {}", e)
                }
                DaemonError::Rpc(code, e) => {
                    write!(f, "[{:?}] {}", code, e)
                }
            },
            Self::Unexpected(e) => write!(f, "Unexpected error: {}", e),
        }
    }
}

impl From<ConfigError> for Error {
    fn from(error: ConfigError) -> Self {
        Error::Config(error.to_string())
    }
}

impl From<DaemonError> for Error {
    fn from(error: DaemonError) -> Self {
        Error::Daemon(error)
    }
}
