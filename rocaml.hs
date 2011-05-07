import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr
     deriving Show
data Unop = Not deriving Show
data Duop = And | Iff deriving Show
data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt
          | Seq [Stmt]
     deriving Show

opChar = oneOf "=<>@^|&+-*/$%!?~.:"

def = emptyDef{ commentState = "(*"
              , commentEnd = "*)"
              , identStart = letter <|> char '_'
              , identLetter = alphaNum <|> oneOf "_'"
              , opStart = opChar
              , opLetter = opChar
              , reservedNames = ["and", "as", "assert", "asr", "begin", "class"
                                ,"constraint", "do", "done", "downto", "else"
                                ,"end", "exception", "external", "false", "for"
                                ,"fun", "function", "functor", "if", "in"
                                ,"include", "inherit", "initializer", "land"
                                ,"lazy", "let", "lor", "lsl", "lsr", "lxor"
                                ,"match", "method", "mod", "module", "mutable"
                                ,"new", "object", "of", "open", "or", "private"
                                ,"rec", "sig", "struct", "then", "to", "true"
                                ,"try", "type", "val", "virtual", "when"
                                ,"while", "with", "parser"
                                ]
              , reservedOpNames = ["!=","#","&","&&","'","(",")","*","+",","
                                  ,"-","-.","->",".","..",":","::",":=",":>"
                                  ,";",";;","<","<-","=",">",">]",">}","?","??"
                                  ,"[","[<","[>","[|","]","_","`","{","{<","|"
                                  ,"|]","}","~","<<","<:",">>","$","$$","$:"
                                  ]
              }

TokenParser { parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Infix (m_reservedOp "+" >> return add)]
        , [Infix (m_reservedOp "-" >> return sub)]
        , [Infix (m_reservedOp "*" >> return mul)]
        , [Infix (m_reservedOp "/" >> return div)]
        , [Infix (m_reservedOp "+" >> m_reservedOp "." >> return addf)]
        ]
term = m_parens exprparser
       <|> fmap var m_identifier
       <|> (m_reserved "true" >> return True)
       <|> (m_reserved "false" >> return False)

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
           where
                stmtparser :: Parser Stmt
                stmtparser = fmap seq (m_semiSep1 stmt1)
                stmt1 = (m_reserved "nop" >> return Nop)
                        <|> do { v <- m_identifier
                               ; m_reservedOp ":="
                               ; e <- exprparser
                               ; return (v := e)
                               }
                        <|> do { m_reserved "if"
                               ; b <- exprparser
                               ; m_reserved "then"
                               ; p <- stmtparser
                               ; m_reserved "else"
                               ; q <- stmtparser
                               ; m_reserved "fi"
                               ; return (If b p q)
                               }
                        <|> do { m_reserved "while"
                               ; b <- exprparser
                               ; m_reserved "do"
                               ; p <- stmtparser
                               ; m_reserved "od"
                               ; return (While b p)
                               }

play :: String -> IO ()
play inp = case parse mainparser "" inp of
           { Left err -> print err
           ; Right ans -> print ans
           }