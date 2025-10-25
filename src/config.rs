use crate::{error::CompileError, prelude::*};

pub static mut CONFIG: Option<Config> = None;

pub fn config() -> &'static Config {
    unsafe { CONFIG.as_ref().unwrap() }
}

#[derive(Debug)]
pub struct Config {
    package:          Package,
    dependencies:     Vec<Dependency>,
    dev_dependencies: Vec<Dependency>,
    root:             std::path::PathBuf,
}

impl Config {
    pub fn package(&self) -> &Package {
        &self.package
    }

    pub fn dependencies(&self) -> &[Dependency] {
        &self.dependencies
    }

    pub fn dev_dependencies(&self) -> &[Dependency] {
        &self.dev_dependencies
    }

    pub fn root(&self) -> &std::path::PathBuf {
        &self.root
    }
}

#[derive(Debug)]
pub struct Package {
    name:    String,
    version: Version,
}

impl Package {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn version(&self) -> &Version {
        &self.version
    }
}

#[derive(Debug)]
pub struct Dependency {
    name:    String,
    version: Version,
}

#[derive(Debug)]
pub struct Version {
    major:       u32,
    minor:       u32,
    patch:       u32,
    pre_release: Option<Vec<String>>,
    metadata:    Option<Vec<String>>,
}

impl FromStr for Version {
    type Err = CompileError;

    fn from_str(version: &str) -> Result<Self, Self::Err> {
        let first_period = version.find('.').ok_or(CompileError::InvalidConfig(
            "version must be in the format x.y.z",
        ))?;
        let major = version[..first_period]
            .parse::<u32>()
            .map_err(|_| CompileError::InvalidConfig("major version must be a number"))?;
        let version = &version[first_period + 1..];
        let second_period = version.find('.').ok_or(CompileError::InvalidConfig(
            "version must be in the format x.y.z",
        ))?;
        let minor = version[..second_period]
            .parse::<u32>()
            .map_err(|_| CompileError::InvalidConfig("minor version must be a number"))?;
        let version = &version[second_period + 1..];
        let third_period = version.find('.').unwrap_or(version.len());
        let patch = version[..third_period]
            .parse::<u32>()
            .map_err(|_| CompileError::InvalidConfig("patch version must be a number"))?;
        let version = &version[third_period..];
        let pre_release = if let Some(pre_release) = version.strip_prefix('-') {
            // dot separated ascii letters, numbers, and dashes until a + or end of line
            let pre_release_end = pre_release.find('+').unwrap_or(pre_release.len());
            let pre_release = &pre_release[..pre_release_end];
            let split = pre_release.split('.').collect::<Vec<_>>();
            for part in &split {
                if part.is_empty() {
                    return Err(CompileError::InvalidConfig(
                        "pre-release version must be a dot separated list of ascii letters, \
                         numbers, and dashes",
                    ));
                }
                if part.chars().any(|c| !c.is_ascii_alphanumeric() && c != '-') {
                    return Err(CompileError::InvalidConfig(
                        "pre-release version must be a dot separated list of ascii letters, \
                         numbers, and dashes",
                    ));
                }
            }
            if split.is_empty() {
                return Err(CompileError::InvalidConfig(
                    "pre-release version must be a dot separated list of ascii letters, numbers, \
                     and dashes",
                ));
            }
            Some(split.iter().map(|s| s.to_string()).collect())
        } else {
            None
        };
        let metadata = if let Some(metadata) = version.strip_prefix('+') {
            // dot separated ascii letters, numbers, and dashes until end of line
            let split = metadata.split('.').collect::<Vec<_>>();
            for part in &split {
                if part.is_empty() {
                    return Err(CompileError::InvalidConfig(
                        "metadata version must be a dot separated list of ascii letters, numbers, \
                         and dashes",
                    ));
                }
                if part.chars().any(|c| !c.is_ascii_alphanumeric() && c != '-') {
                    return Err(CompileError::InvalidConfig(
                        "metadata version must be a dot separated list of ascii letters, numbers, \
                         and dashes",
                    ));
                }
            }
            if split.is_empty() {
                return Err(CompileError::InvalidConfig(
                    "metadata version must be a dot separated list of ascii letters, numbers, and \
                     dashes",
                ));
            }
            Some(split.iter().map(|s| s.to_string()).collect())
        } else {
            None
        };
        Ok(Version {
            major,
            minor,
            patch,
            pre_release,
            metadata,
        })
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;
        if let Some(pre_release) = &self.pre_release {
            write!(f, "-")?;
            for (i, part) in pre_release.iter().enumerate() {
                if i > 0 {
                    write!(f, ".")?;
                }
                write!(f, "{}", part)?;
            }
        }
        if let Some(metadata) = &self.metadata {
            write!(f, "+")?;
            for (i, part) in metadata.iter().enumerate() {
                if i > 0 {
                    write!(f, ".")?;
                }
                write!(f, "{}", part)?;
            }
        }
        Ok(())
    }
}

/// Search all parent directories for a file named bay.toml
pub fn get_config_path() -> Result<std::path::PathBuf> {
    let Ok(mut path) = std::env::current_dir() else {
        return Err(CompileError::ConfigNotFound);
    };
    loop {
        let config_path = path.join("bay.toml");
        if config_path.exists() {
            return Ok(config_path);
        }
        if !path.pop() {
            return Err(CompileError::ConfigNotFound);
        }
    }
}

impl Config {
    pub fn read() -> Result<Config> {
        let path = get_config_path()?;
        let Ok(str) = std::fs::read_to_string(&path) else {
            return Err(CompileError::ConfigNotFound);
        };
        let config = str.parse::<toml::Value>()?;
        let Some(package_value) = config.get("package") else {
            return Err(CompileError::InvalidConfig("missing `package` section"));
        };
        let package = Package {
            name:    match package_value.get("name") {
                Some(toml::Value::String(name)) => {
                    if name
                        .chars()
                        .any(|c| !c.is_ascii_alphanumeric() && c != '-' && c != '_')
                    {
                        return Err(CompileError::InvalidConfig(
                            "project name must be alphanumeric, dashes, or underscores",
                        ));
                    }
                    name.clone()
                },
                _ => {
                    return Err(CompileError::InvalidConfig(
                        "missing `name` field in `package` section",
                    ))
                },
            },
            version: package_value
                .get("version")
                .and_then(|v| v.as_str())
                .ok_or(CompileError::InvalidConfig(
                    "missing `version` field in `package` section",
                ))?
                .parse::<Version>()?,
        };
        let mut dependencies = Vec::new();
        let empty_table = toml::Value::Table(toml::map::Map::new());
        let Some(deps) = config
            .get("dependencies")
            .unwrap_or(&empty_table)
            .as_table()
        else {
            return Err(CompileError::InvalidConfig(
                "missing `dependencies` section",
            ));
        };
        for (name, value) in deps {
            let version = match value {
                toml::Value::String(version) => Version::from_str(version)?,
                toml::Value::Table(table) => {
                    if let Some(version) = table.get("version") {
                        Version::from_str(version.as_str().unwrap_or_default())?
                    } else {
                        return Err(CompileError::InvalidConfig(
                            "missing `version` field in `dependencies` section",
                        ));
                    }
                },
                _ => {
                    return Err(CompileError::InvalidConfig(
                        "invalid `dependencies` section",
                    ))
                },
            };
            dependencies.push(Dependency {
                name: name.clone(),
                version,
            });
        }
        let mut dev_dependencies = Vec::new();
        let Some(dev_deps) = config
            .get("dev-dependencies")
            .unwrap_or(&empty_table)
            .as_table()
        else {
            return Err(CompileError::InvalidConfig(
                "missing `dev-dependencies` section",
            ));
        };
        for (name, value) in dev_deps {
            let version = match value {
                toml::Value::String(version) => Version::from_str(version)?,
                toml::Value::Table(table) => {
                    if let Some(version) = table.get("version") {
                        Version::from_str(version.as_str().unwrap_or_default())?
                    } else {
                        return Err(CompileError::InvalidConfig(
                            "missing `version` field in `dev-dependencies` section",
                        ));
                    }
                },
                _ => {
                    return Err(CompileError::InvalidConfig(
                        "invalid `dev-dependencies` section",
                    ))
                },
            };
            dev_dependencies.push(Dependency {
                name: name.clone(),
                version,
            });
        }
        Ok(Config {
            package,
            dependencies,
            dev_dependencies,
            root: path.parent().unwrap_or(&path).to_path_buf(),
        })
    }

    #[allow(dead_code)]
    pub fn to_toml(&self) -> Result<String> {
        let mut toml = toml::map::Map::new();
        let mut package = toml::map::Map::new();
        package.insert(
            "name".to_string(),
            toml::Value::String(self.package.name.clone()),
        );
        package.insert(
            "version".to_string(),
            toml::Value::String(self.package.version.to_string()),
        );
        let package = toml::Value::Table(package);
        let mut dependencies = toml::map::Map::new();
        for dep in &self.dependencies {
            let mut dep_table = toml::map::Map::new();
            dep_table.insert(
                "version".to_string(),
                toml::Value::String(dep.version.to_string()),
            );
            dependencies.insert(dep.name.clone(), toml::Value::Table(dep_table));
        }
        let dependencies = toml::Value::Table(dependencies);
        let mut dev_dependencies = toml::map::Map::new();
        for dep in &self.dev_dependencies {
            let mut dep_table = toml::map::Map::new();
            dep_table.insert(
                "version".to_string(),
                toml::Value::String(dep.version.to_string()),
            );
            dev_dependencies.insert(dep.name.clone(), toml::Value::Table(dep_table));
        }
        let dev_dependencies = toml::Value::Table(dev_dependencies);
        Ok(format!(
            "[package]\n{}\n[dependencies]\n{}\n[dev-dependencies]\n{}",
            toml::ser::to_string_pretty(&package)
                .map_err(|_| CompileError::InvalidConfig("failed to serialize config"))?,
            toml::ser::to_string_pretty(&dependencies)
                .map_err(|_| CompileError::InvalidConfig("failed to serialize config"))?,
            toml::ser::to_string_pretty(&dev_dependencies)
                .map_err(|_| CompileError::InvalidConfig("failed to serialize config"))?,
        ))
    }

    pub fn new_project(name: &str) -> Result<()> {
        let current_dir = std::env::current_dir()?;
        if name
            .chars()
            .any(|c| !c.is_ascii_alphanumeric() && c != '-' && c != '_')
        {
            return Err(CompileError::InvalidConfig(
                "project name must be alphanumeric, dashes, or underscores",
            ));
        }
        let name = name.replace('-', "_");
        let project_dir = current_dir.join(&name);
        if project_dir.exists() {
            return Err(CompileError::InvalidConfig(
                "project directory already exists",
            ));
        }
        std::fs::create_dir(&project_dir)?;
        let src_dir = project_dir.join("src");
        std::fs::create_dir(&src_dir)?;
        let main_file = src_dir.join("main.bay");
        std::fs::write(
            &main_file,
            r#"fn main() {
    print_i32(42);
}"#,
        )?;
        let config = Config {
            package:          Package {
                name:    name.to_string(),
                version: Version {
                    major:       0,
                    minor:       1,
                    patch:       0,
                    pre_release: None,
                    metadata:    None,
                },
            },
            dependencies:     Vec::new(),
            dev_dependencies: Vec::new(),
            root:             project_dir.clone(),
        };
        let toml = config.to_toml()?;
        let config_file = project_dir.join("bay.toml");
        std::fs::write(&config_file, toml)?;
        Ok(())
    }
}
