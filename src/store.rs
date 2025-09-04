use core::str;

use dashmap::DashMap;

use crate::prelude::*;

pub static mut FILE_STORE: Option<FileStore> = None;

pub struct FileStore {
    pub files: DashMap<u64, (std::path::PathBuf, String)>,
}

impl FileStore {
    pub fn new() -> Self {
        Self {
            files: DashMap::new(),
        }
    }

    pub fn add_file(&mut self, path: std::path::PathBuf, content: String) -> u64 {
        let id = rand::random::<u64>();
        self.files.insert(id, (path, content));
        id
    }

    pub fn get_path(&self, id: u64) -> &std::path::Path {
        self.files
            .get(&id)
            .map(|x| unsafe { &*(x.0.as_path() as *const std::path::Path) })
            .expect("File not found")
    }

    pub fn get_content(&self, id: u64) -> &'static str {
        self.files
            .get(&id)
            .map(|x| unsafe {
                str::from_utf8_unchecked(std::slice::from_raw_parts(x.1.as_ptr(), x.1.len()))
            })
            .expect("File not found")
    }
}

pub fn add_file(path: impl Into<std::path::PathBuf>) -> Result<(u64, &'static str)> {
    let path = path.into();
    let content = std::fs::read_to_string(&path)?;
    unsafe {
        let id = FILE_STORE.as_mut().unwrap().add_file(path, content);
        Ok((id, FILE_STORE.as_ref().unwrap().get_content(id)))
    }
}

pub fn get_file_path(id: u64) -> &'static std::path::Path {
    unsafe { FILE_STORE.as_ref().unwrap().get_path(id) }
}

pub fn get_file_content(id: u64) -> &'static str {
    unsafe { FILE_STORE.as_ref().unwrap().get_content(id) }
}
