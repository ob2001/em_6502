use crate::prelude::*;

pub struct Mem { mem: [CPUByte; CPU_MEMSIZE], debug_bytes_per_line: usize }
pub type MemResult = Result<Mem, String>;

impl Mem {
    /// Create new CPU memory with all bytes set to the provided value
    pub fn new_all(b: CPUByte) -> Self {
        Self { mem: [b; CPU_MEMSIZE], debug_bytes_per_line: 24 }
    }

    /// Create new cpu memory with all NOP instructions
    pub fn new_nops() -> Self {
        Self::new_all(0xEA)
    }

    /// Create new cpu memory with all (*illegal) HLT instructions
    pub fn new_hlts() -> Self {
        Self::new_all(0xFF)
    }

    /// Create new zeroed cpu memory with `code` inserted in range `mem[idx]` - `mem[idx + code.len()`.
    pub fn new_nops_with_code_at(code: &[CPUByte], idx: usize) -> MemResult {
        if idx > CPU_MEMSIZE {
            Err("Error: code insertion point out of memory bounds".to_string())
        } else if code.len() + idx > CPU_MEMSIZE {
            Err("Error: code overflows memory bounds. Consider inserting at an earlier index".to_string())
        } else {
            let mut ret = Self::new_nops();

            for (offset, &byte) in code.iter().enumerate() {
                ret.mem[idx + offset] = byte;
            }

            Ok(ret)
        }
    }

    /// Create new cpu memory of all HLTs with `code` inserted in range `mem[idx]` - `mem[idx] + code.len()`.
    pub fn new_hlts_with_code_at(code: &[CPUByte], idx: usize) -> MemResult {
        if idx > CPU_MEMSIZE {
            Err("Error: code insertion point out of memory bounds".to_string())
        } else if code.len() + idx > CPU_MEMSIZE {
            Err("Error: code overflows memory bounds. Consider inserting at an earlier index".to_string())
        } else {
            let mut ret = Self::new_hlts();

            for (offset, &byte) in code.iter().enumerate() {
                ret.mem[idx + offset] = byte;
            }

            Ok(ret)
        }
    }

    /// Generate CPU memory from a file specification
    pub fn new_from_file(file: String) -> MemResult {
        if let Ok(f) = std::fs::read_to_string(&file) {
            let mut ret = Self::new_nops();
            let mut byte_radix_mode: u32 = 16;

            for (i, line) in f.lines().enumerate() {
                let trim_line = line.trim();

                // Ensure that line contains at least one non-comment, non-whitespace character.
                if trim_line.len() == 0 || trim_line.chars().next() == Some(';') { continue }

                // Discard comments and the trim the non-comment section of whitespace characters.
                let trim_line = line.trim().split(';').next().expect(&format!("Parsing error: should be able to split line {} on \';\'", i + 1)).trim();

                // We only need the strings on either side of the first colon.
                // Discard any second colon along with whatever may come after it.
                let mut split_line = trim_line.split(':');

                // Store our predicate in pre and match to determine how to process post.
                let (pre, mut post) = (split_line.next().unwrap().trim(), split_line.next().unwrap().split(',').map(str::trim));

                match pre {
                    "byte_radix_mode" => {
                        let val = post.next().unwrap();
                        byte_radix_mode = match val.to_lowercase().as_str() {
                            "2" | "bin" | "binary" => 2,
                            "8" | "oct" | "octal" => 8,
                            "10" | "dec" | "decimal" => 10,
                            "16" | "hex" | "hexadecimal" => 16,
                            _ => panic!("Error parsing byte_radix_mode in line {}. Only support binary, octal, decimal, or hexadecimal modes.", i + 1),
                        }
                    },
                    "fill_byte" => {
                        let fill_byte = post.next().unwrap();
                        let fill_byte = u8::from_str_radix(fill_byte, byte_radix_mode).expect(&format!("Error parsing fill_byte in line {}: \"{}\" in an invalid base-{byte_radix_mode} representation of a byte", i + 1, fill_byte));
                        ret = Self::new_all(fill_byte);
                    },
                    addr => {
                        let address = u16::from_str_radix(addr, 16).expect(&format!("Error parsing address predicate in line {}: \"{}\" is an invalid hexadecimal representation of 2 bytes", i + 1, addr));
                        let mut mem_line: Vec<u8> = Vec::new();
                        
                        for byte in post {
                            let byte = byte.trim();
                            let byte = u8::from_str_radix(byte, byte_radix_mode).expect(&format!("Error parsing byte in line {}: \"{}\" is an invalid base-{byte_radix_mode} representation of a byte", i + 1, byte));
                            mem_line.push(byte);
                        }

                        ret.insert_code_at(&mem_line, address as usize)?;
                    }
                }
            }

            Ok(ret)
        } else {
            Err(format!("Error reading supplied memory specification file: {}", file))
        }
    }

    /// Modify existing cpu memory `mem` by inserting `code` in range `mem[idx]` - `mem[idx + code.len()]`.
    ///
    /// Will overwrite memory in modification range.
    pub fn insert_code_at(&mut self, code: &[CPUByte], idx: usize) -> Result<(), &'static str> {
        if idx > CPU_MEMSIZE {
            Err("Error: code insertion point out of memory bounds")
        } else if code.len() + idx > CPU_MEMSIZE {
            Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
        } else {
            for (offset, &byte) in code.iter().enumerate() {
                self.mem[idx + offset] = byte;
            }
            Ok(())
        }
    }

    /// Return the value of the byte at the provided memory address
    pub fn byte_at(&self, addr: CPUWord) -> CPUByte {
        self.mem[addr as usize]
    }

    /// Return mutable reference to the byte stored at the provided memory address
    pub fn mut_byte_at(&mut self, addr: CPUWord) -> &mut CPUByte {
        &mut self.mem[addr as usize]
    }

    /// Return the entire contents of the structure
    pub fn get_all(&self) -> &[CPUByte; CPU_MEMSIZE] {
        &self.mem
    }

    pub fn set_debug_bytes_per_line(&mut self, dbpl: usize) {
        self.debug_bytes_per_line = dbpl;
    }
}

impl std::fmt::Debug for Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n{:-^1$}\n", "!!! MEMORY DUMP !!!", self.debug_bytes_per_line * 4 + 8)?;
        for (i, &byte) in self.mem.iter().enumerate() {
            if i % self.debug_bytes_per_line == 0 {
                write!(f, "[{:#06X}]: ", i)?;
            }

            if (i + 1) % self.debug_bytes_per_line != 0 {
                write!(f, "{:02X}, ", byte)?;
            } else {
                write!(f, "{:02X}\n", byte)?;
            }
        }

        Ok(())
    }
}