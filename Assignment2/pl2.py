import ply.yacc as yacc
import ply.lex as lex

# List of all token names
tokens = [
    'IDENTIFIER', 'NUMBER',
    'IF', 'ELSE', 'WHILE', 'LET', 'LOOP', 'FN', 'MUT', 'PUB', 'REF', 'IN',
    'STRUCT', 'TYPE', 'RETURN', 'TRUE', 'FALSE', 'WHERE', 'WRITE',
    'LPAREN', 'RPAREN', 'LCURLY', 'RCURLY', 'LSQR', 'RSQR', 'SEMICOLON', 'COMMA',
    'EQUALS', 'NEQ', 'LEQ', 'GEQ', 'LT', 'GT', 'PLUS', 'MINUS', 'STAR', 'SLASH', 'MOD', 'INT', 'FLOAT', 'CHAR', 'BOOLEAN',
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
t_INT = r'int'
t_FLOAT = r'float'
t_CHAR = r'char'
t_BOOLEAN = r'boolean'

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
    r'\d+'  # same thing as [0-9]+
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

# ==================================================================
#                        PARSING RULES
# ==================================================================

# Define the precedence of operators
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'STAR', 'SLASH', 'MOD'),
    ('nonassoc', 'LEQ', 'GEQ', 'LT', 'GT'),
    ('nonassoc', 'EQUALS', 'NEQ'),
)

# Parser rules


def p_program(p):
    '''program : program function
               | program struct
               | function
               | struct'''
    if len(p) == 2:
        p[0] = ('program', [p[1]])
    else:
        p[0] = ('program', p[1][1] + [p[2]])


def p_function(p):
    '''function : FN IDENTIFIER LPAREN list_parameters RPAREN return_type LCURLY statements RCURLY'''
    p[0] = ('function', p[2], p[4], p[6], p[8])


def p_return_type(p):
    '''return_type : type
                   | empty'''
    p[0] = p[1]


def p_statements_single(p):
    '''statements : statement'''
    p[0] = [p[1]]


def p_statements_multiple(p):
    '''statements : statements statement'''
    p[0] = p[1] + [p[2]]


def p_statement_assignment(p):
    '''statement : assignment_statement'''
    p[0] = p[1]


def p_statement_if(p):
    '''statement : if_statement'''
    p[0] = p[1]


def p_statement_while(p):
    '''statement : while_statement'''
    p[0] = p[1]


def p_statement_action(p):
    '''statement : action_statement'''
    p[0] = p[1]


def p_statement_let(p):
    '''statement : let_expression'''
    p[0] = p[1]


def p_assignment_statement(p):
    '''assignment_statement : IDENTIFIER EQUALS expression SEMICOLON'''
    p[0] = ('assign', p[1], p[3])


def p_if_statement(p):
    '''if_statement : IF expression LCURLY statements RCURLY else_clause'''
    p[0] = ('if', p[2], p[4], p[6])


def p_else_clause(p):
    '''else_clause : ELSE LCURLY statements RCURLY
                   | ELSE IF expression LCURLY statements RCURLY else_clause
                   | empty'''
    if len(p) == 5:
        p[0] = ('else', p[3])
    elif len(p) == 8:
        p[0] = ('else_if', p[3], p[5], p[7])
    else:
        p[0] = None


def p_while_statement(p):
    '''while_statement : WHILE LPAREN expression RPAREN LCURLY statements RCURLY'''
    p[0] = ('while', p[3], p[6])


def p_action_statement_return(p):
    '''action_statement : RETURN expression SEMICOLON'''
    p[0] = ('return', p[2])


def p_action_statement_write(p):
    '''action_statement : WRITE expression SEMICOLON'''
    p[0] = ('write', p[2])


def p_action_statement_where(p):
    '''action_statement : WHERE expression SEMICOLON'''
    p[0] = ('where', p[2])


def p_action_statement_loop(p):
    '''action_statement : LOOP expression SEMICOLON'''
    p[0] = ('loop', p[2])


def p_let_expression(p):
    '''let_expression : LET IDENTIFIER EQUALS expression SEMICOLON
                      | LET MUT IDENTIFIER EQUALS expression SEMICOLON
                      | LET REF IDENTIFIER EQUALS expression SEMICOLON'''
    if len(p) == 6:
        p[0] = ('let', p[2], p[4])
    elif p[1] == 'let' and p[2] == 'mut':
        p[0] = ('let_mut', p[3], p[5])
    else:
        p[0] = ('let_ref', p[3], p[5])


def p_expression_identifier(p):
    '''expression : IDENTIFIER'''
    p[0] = ('identifier', p[1])


def p_expression_operation(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression STAR expression
                  | expression SLASH expression
                  | expression MOD expression
                  | expression NEQ expression
                  | expression LEQ expression
                  | expression GEQ expression
                  | expression LT expression
                  | expression GT expression'''
    p[0] = ('operation', p[2], p[1], p[3])


def p_expression_paren(p):
    '''expression : LPAREN expression RPAREN'''
    p[0] = p[2]


def p_expression_number(p):
    '''expression : NUMBER'''
    p[0] = ('number', p[1])


def p_struct(p):
    '''struct : STRUCT IDENTIFIER LCURLY struct_statements RCURLY'''
    p[0] = ('struct', p[2], p[4])


def p_struct_statements(p):
    '''struct_statements : let_expression
                         | struct_statements let_expression'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]


def p_list_parameters_empty(p):
    '''list_parameters : '''
    p[0] = []


def p_list_parameters_single(p):
    '''list_parameters : IDENTIFIER type'''
    p[0] = [(p[1], p[2])]


def p_list_parameters_multiple(p):
    '''list_parameters : IDENTIFIER type COMMA list_parameters'''
    p[0] = [(p[1], p[2])] + p[4]


def p_type(p):
    '''type : INT
            | FLOAT
            | CHAR
            | BOOLEAN
            | IDENTIFIER'''
    p[0] = p[1]


def p_empty(p):
    'empty :'
    pass


def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}', line {p.lineno}")
    else:
        print("Syntax error at EOF")


# Build the parser
parser = yacc.yacc()


def parse(data):
    print("Parsing:")
    return parser.parse(data)


def print_parse_tree(tree, indent=0):
    """
    Prints the parse tree in a tree-like structure/format

    :param tree: The parse tree to print
    :param indent: The indentation level
    """
    if isinstance(tree, tuple):
        print('    ' * indent + str(tree[0]) + ':')
        for child in tree[1:]:
            print_parse_tree(child, indent + 1)
    elif isinstance(tree, list):
        for item in tree:
            print_parse_tree(item, indent)
    else:
        print('    ' * indent + str(tree))


if __name__ == "__main__":
    with open("Program_Test.txt", 'r') as tester:
        input_data = tester.read()
        lexer.input(input_data)

        headers = ['Line', 'Token', 'Value']  # for format output
        print(f'{headers[0]:4} {headers[1]:15} {headers[2]}')
        print("-"*27)
        while True:
            token = lexer.token()
            if not token:
                break
            print(f'{token.lineno:4} {token.type:15} {token.value}')
        print("\n")

        result = parse(input_data)
        print("\nParse Tree:\n")
        print_parse_tree(result)
