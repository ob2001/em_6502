use crate::prelude::*;

pub type MemResult = Result<[CPUByte; CPU_MEMSIZE], &'static str>;

/// Create new cpu memory with all bytes set to the provided value
pub fn new_all(b: CPUByte) -> [CPUByte; CPU_MEMSIZE] {
    [b; CPU_MEMSIZE]
}

/// Create new cpu memory with all NOP instructions
pub fn new_nops() -> [CPUByte; CPU_MEMSIZE] {
    new_all(0x00)
}

/// Create new cpu memory with all (illegal) HLT instructions
pub fn new_hlts() -> [CPUByte; CPU_MEMSIZE] {
    new_all(0xFF)
}

/// Return new zeroed cpu memory with `code` inserted in range `mem[idx]` - `mem[idx + code.len()`.
pub fn new_nops_with_code_at(code: &[CPUByte], idx: usize) -> MemResult {
    if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else if code.len() + idx > CPU_MEMSIZE {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    } else {
        let mut ret = new_nops();

        for (offset, &byte) in code.iter().enumerate() {
            ret[idx + offset] = byte;
        }

        Ok(ret)
    }
}

pub fn new_hlts_with_code_at(code: &[CPUByte], idx: usize) -> MemResult {
    if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else if code.len() + idx > CPU_MEMSIZE {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    } else {
        let mut ret = new_hlts();

        for (offset, &byte) in code.iter().enumerate() {
            ret[idx + offset] = byte;
        }

        Ok(ret)
    }
}

/// Modify existing cpu memory `mem` by inserting `code` in range `mem[idx]` - `mem[idx + code.len()]`.
///
/// Will overwrite memory in modification range.
pub fn insert_code_at(mem: &mut [CPUByte; CPU_MEMSIZE], code: &[CPUByte], idx: usize) -> Result<(), &'static str> {
    if idx <= CPU_MEMSIZE && code.len() + idx <= CPU_MEMSIZE {
        for (offset, &byte) in code.iter().enumerate() {
            mem[idx + offset] = byte;
        }
        Ok(())
    } else if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    }
}

