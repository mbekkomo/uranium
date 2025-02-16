import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// A magic number to detect compiled terminfo.
const magic_num_b16 = 0x11A

/// A magic number to enable 32-bit integers.
const magic_num_b32 = 0x21E

/// Identifies the number format used in the compiiled database.
type NumType {
  Bit16
  Bit32
}

/// Header information of the compiled term database.
/// See manpage term(5) for more informations.
type HeaderInfo {
  HeaderInfo(
    /// The bit size of numbers.
    num_type: NumType,
    /// Size (in bytes) of the terminal names section.
    terminal_names_size: Int,
    /// Number of bytes in the boolean flags section.
    boolean_flags_bytes: Int,
    /// Number of short integers in the numbers section.
    numbers_short_ints: Int,
    /// Number of offsets (in short integers) in the strings section.
    strings_offsets: Int,
    /// Size (in bytes) of the string table.
    string_table_size: Int,
  )
}

/// Enums of error produced by functions in this module.
///
/// Some errors might have a different context depends on
/// the function's purpose. 
pub type Error {
  /// Failed to detect a format, usually caused by a function that tries to read
  /// the data format by checking the magic numbers.
  InvalidFormat
  /// The database doesn't contain a name for the terminal. 
  EmptyTerminalName
  /// Assertation has failed, either caused by unmatched data that has been parsed
  /// with the one on the header.
  FailedAssertation
}

/// Informations about the terminal, such as name, aliases,
/// and capabilities that the terminal had.
///
/// See terminfo(5) for all the capabilities.
pub opaque type TermInfo {
  TermInfo(
    /// The name of the terminal.
    name: String,
    /// The aliases of the terminal.
    aliases: List(String),
    /// List of boolean capabilities.
    boolean_capabilities: Dict(String, Bool),
    /// List of number capabilities.
    number_capabilities: Dict(String, Int),
    /// List of string capabilities.
    string_capabilities: Dict(String, String),
    /// List of boolean capabilities extension.
    extended_boolean_capabilities: Dict(String, Bool),
    /// List of number capabilities extension.
    extended_number_capabilities: Dict(String, Int),
    /// List of string capabilities extension.
    extended_string_capabilities: Dict(String, String),
  )
}

fn get_boolean_capability(idx: Int) -> Result(String, Nil) {
  case idx {
    0 -> Ok("auto_left_margin")
    1 -> Ok("auto_right_margin")
    2 -> Ok("back_color_erase")
    3 -> Ok("can_change")
    4 -> Ok("ceol_standout_glitch")
    5 -> Ok("col_addr_glitch")
    6 -> Ok("cpi_changes_res")
    7 -> Ok("cr_cancels_micro_mode")
    8 -> Ok("dest_tabs_magic_smso")
    9 -> Ok("eat_newline_glitch")
    10 -> Ok("erase_overstrike")
    11 -> Ok("generic_type")
    12 -> Ok("hard_copy")
    13 -> Ok("hard_cursor")
    14 -> Ok("has_meta_key")
    15 -> Ok("has_print_wheel")
    16 -> Ok("has_status_line")
    17 -> Ok("hue_lightness_saturation")
    18 -> Ok("insert_null_glitch")
    19 -> Ok("lpi_changes_res")
    20 -> Ok("memory_above")
    21 -> Ok("memory_below")
    22 -> Ok("move_insert_mode")
    23 -> Ok("move_standout_mode")
    24 -> Ok("needs_xon_xoff")
    25 -> Ok("no_esc_ctlc")
    26 -> Ok("no_pad_char")
    27 -> Ok("non_dest_scroll_region")
    28 -> Ok("non_rev_rmcup")
    29 -> Ok("over_strike")
    30 -> Ok("prtr_silent")
    31 -> Ok("row_addr_glitch")
    32 -> Ok("semi_auto_right_margin")
    33 -> Ok("status_line_esc_ok")
    34 -> Ok("tilde_glitch")
    35 -> Ok("transparent_underline")
    36 -> Ok("xon_xoff")
    _ -> Error(Nil)
  }
}

fn get_number_capability(idx: Int) -> Result(String, Nil) {
  case idx {
    0 -> Ok("columns")
    1 -> Ok("init_tabs")
    2 -> Ok("label_height")
    3 -> Ok("label_width")
    4 -> Ok("lines")
    5 -> Ok("lines_of_memory")
    6 -> Ok("magic_cookie_glitch")
    7 -> Ok("max_attributes")
    8 -> Ok("max_colors")
    9 -> Ok("max_pairs")
    10 -> Ok("maximum_windows")
    11 -> Ok("no_color_video")
    12 -> Ok("num_labels")
    13 -> Ok("padding_baud_rate")
    14 -> Ok("virtual_terminal")
    15 -> Ok("width_status_line")
    _ -> Error(Nil)
  }
}

// fn get_string_capability(idx: Int) Result(String, Nil) {
//   case idx {
//     _ -> Error(Nil)
//   }
// }

fn get_ushort16(data: BitArray, at: Int) -> Int {
  case bit_array.slice(data, at, 2) |> result.unwrap(<<>>) {
    <<a:unsigned-little-size(8), b:unsigned-little-size(8), _rest:bytes>> -> {
      let int = 256 * a + b
      let assert <<x:unsigned-little-size(16)>> = <<int:little-size(16)>>
      x
    }
    <<>> | _ -> 0
  }
}

fn detect_header(compiled_terminfo: BitArray) -> Result(HeaderInfo, Error) {
  let num_type = case get_ushort16(compiled_terminfo, 0) {
    x if x == magic_num_b16 -> Some(Bit16)
    x if x == magic_num_b32 -> Some(Bit32)
    _ -> None
  }

  case num_type {
    Some(_) -> {
      let name_len = get_ushort16(compiled_terminfo, 2)
      let bool_len = get_ushort16(compiled_terminfo, 4)
      let num_len = get_ushort16(compiled_terminfo, 6)
      let strings_len = get_ushort16(compiled_terminfo, 8)
      let table_size = get_ushort16(compiled_terminfo, 10)

      case { bit_array.byte_size(compiled_terminfo) - 12 } > name_len {
        True ->
          Ok(HeaderInfo(
            option.unwrap(num_type, Bit16),
            name_len,
            bool_len,
            num_len,
            strings_len,
            table_size,
          ))
        False -> Error(InvalidFormat)
      }
    }
    None -> Error(InvalidFormat)
  }
}

fn read_termnames_until(data: BitArray, at: Int) -> BitArray {
  case bit_array.slice(data, at, 1) |> result.unwrap(<<>>) {
    <<x:unsigned-little-size(8)>> ->
      case x {
        0 -> bit_array.slice(data, 0, at) |> result.unwrap(<<>>)
        _ -> read_termnames_until(data, at + 1)
      }
    _ | <<>> -> data
  }
}

fn read_terminal_names(
  data: BitArray,
  term_info: TermInfo,
  header_info: HeaderInfo,
) -> Result(TermInfo, Error) {
  let terminal_names = read_termnames_until(data, 0)
  case bit_array.byte_size(terminal_names) == header_info.terminal_names_size {
    True ->
      case
        bit_array.to_string(terminal_names)
        |> result.unwrap("")
        |> string.split("|")
      {
        [name, ..aliases] ->
          case name {
            "" -> Error(EmptyTerminalName)
            _ -> Ok(TermInfo(..term_info, name:, aliases:))
          }
        [] -> Error(EmptyTerminalName)
      }
    False -> Error(FailedAssertation)
  }
}
