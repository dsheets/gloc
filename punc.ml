(* Copyright (c) 2011 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

type tok =
    LEFT_OP | RIGHT_OP | INC_OP | DEC_OP | LE_OP | GE_OP | EQ_OP | NE_OP
  | AND_OP | OR_OP | XOR_OP | MUL_ASSIGN | DIV_ASSIGN | ADD_ASSIGN | MOD_ASSIGN
  | LEFT_ASSIGN | RIGHT_ASSIGN | AND_ASSIGN | XOR_ASSIGN | OR_ASSIGN
  | SUB_ASSIGN | LEFT_PAREN | RIGHT_PAREN | LEFT_BRACKET | RIGHT_BRACKET
  | LEFT_BRACE | RIGHT_BRACE | DOT | COMMA | COLON | EQUAL | SEMICOLON | BANG
  | DASH | TILDE | PLUS | STAR | SLASH | PERCENT | LEFT_ANGLE | RIGHT_ANGLE
  | VERTICAL_BAR | CARET | AMPERSAND | QUESTION
