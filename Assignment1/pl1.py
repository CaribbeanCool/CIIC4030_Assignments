import ply.lex as lex

# List of all token names
tokens = [
    'IDENTIFIER', 'NUMBER',
    'IF', 'ELSE', 'WHILE', 'LET', 'LOOP', 'FN', 'MUT', 'PUB', 'REF', 'IN',
    'STRUCT', 'TYPE', 'RETURN', 'TRUE', 'FALSE', 'WHERE', 'WRITE',
    'LPAREN', 'RPAREN', 'LCURLY', 'RCURLY', 'LSQR', 'RSQR', 'SEMICOLON', 'COMMA',
    'EQUALS', 'NEQ', 'LEQ', 'GEQ', 'LT', 'GT', 'PLUS', 'MINUS', 'STAR', 'SLASH', 'MOD'
]

# Regular expression rules for simple tokens
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LCURLY = r'\{'
t_RCURLY = r'\}'
t_LSQR = r'\['
t_RSQR = r'\]'
t_SEMICOLON = r';'
t_COMMA = r','
t_EQUALS = r'='
t_NEQ = r'!='
t_LEQ = r'<='
t_GEQ = r'>='
t_LT = r'<'
t_GT = r'>'
t_PLUS = r'\+'
t_MINUS = r'-'
t_STAR = r'\*'
t_SLASH = r'/'
t_MOD = r'%'

# Define reserved keywords
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'let': 'LET',
    'loop': 'LOOP',
    'fn': 'FN',
    'mut': 'MUT',
    'pub': 'PUB',
    'ref': 'REF',
    'in': 'IN',
    'struct': 'STRUCT',
    'type': 'TYPE',
    'return': 'RETURN',
    'true': 'TRUE',
    'false': 'FALSE',
    'where': 'WHERE',
    'write': 'WRITE',
}

# A function to match identifiers (ID) and keywords


def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')  # Check for reserved keywords
    return t


# A function to match numbers
def t_NUMBER(t):
    r'\d+' # same thing as [0-9]+
    t.value = int(t.value)
    return t


# A function to track line numbers
def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A rule to ignore comments (//) and everything until the end of line


def t_COMMENT(t):
    r'//.*'
    pass


# A rule to ignore whitespace (space, tab, carriage return)
t_ignore = ' \t\r'

# Error handling rule


def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)


# Constructing the lexer
lexer = lex.lex()


with open("Program_Test.txt", 'r') as testFile:
    input_data = testFile.read()
    lexer.input(input_data)
    headers = ['Line', 'Token', 'Value']  # for format output
    print(f'{headers[0]:4} {headers[1]:15} {headers[2]}')
    print("-"*27)
    while True:
        token = lexer.token()
        if not token:
            break
        print(f'{token.lineno:4} {token.type:15} {token.value}')

# def lexerScannerTestFunc(fileDir):
#     """
#     Function to test a given `.txt` file using a lexical scanner

#     Params:
#         fileDir (str): The directory of the file to be tested
#     """
#     with open(fileDir, 'r') as f:
#         input_data = f.read()
#     lexer.input(input_data)


# lexerScannerTestFunc('Assignment1/Program_Test.txt')
