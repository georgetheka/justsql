import datetime
from typing import Callable, Any, List, Optional

from justsql.const import postgres_reserved_words, postgres_unreserved_keywords, \
    postgres_reserved_can_use_as_func_or_type_words, postgres_unreserved_cannot_use_as_func_or_type_words, PyToSqlType


class PySqlTypeContainer:
    """Encapsulates a type container.
    """
    def __init__(self, value, py_to_sql: PyToSqlType):
        self.value = value
        self.py_to_sql = py_to_sql


# todo: test coverage
class Table:
    """Represents a SQL Table.
    """
    def __getattr__(self, name: str):
        return self.__dict__['_' + name].value

    def __setattr__(self, key, value):
        if isinstance(value, PySqlTypeContainer):
            self.__dict__[key] = value
        else:
            self.__dict__['_' + key].value = value

    def _make(self, name: str, py_to_sql: PyToSqlType):
        self.__dict__['_' + name] = PySqlTypeContainer(None, py_to_sql)


class Token:
    """Represents a single identifier or literal in a SQL query."""
    def __init__(self, value: str = None, kind: str = None, prev: 'Token' = None, next: 'Token' = None,
                 is_reserved: bool = False, is_capitalized: bool = False, is_uppercase: bool = False, str_value=None):
        self.value = value
        self.kind = kind
        self.prev = prev
        self.next = next
        self.is_reserved_word = is_reserved
        self.is_capitalized = is_capitalized
        self.is_uppercase = is_uppercase
        self.str_value = str_value


# todo: test coverage
class SQL:
    """The main class that encapsulates SQL methods."""
    @staticmethod
    def _token_kind_identifier(token: Token) -> Token:
        token.str_value = token.value
        return token

    @staticmethod
    def _token_kind_table_or_alias(token: Token) -> Token:
        token.str_value = f'"{token.value}"' if token.is_reserved_word else token.value
        if token.next is not None:
            next_token = token.next
            if next_token.kind == 'column_or_alias':
                new_next = Token(value='.', kind='syntax')
                new_next.next = next_token
                next_token.prev = new_next
                token.next = new_next
        return token

    @staticmethod
    def _token_kind_column_or_alias(token: Token) -> Token:
        token.str_value = f'"{token.value}"' if token.is_reserved_word else token.value
        return token

    @staticmethod
    def _token_kind_syntax(token: Token) -> Token:
        if token.value == ',':
            next_token = token.next
            if (next_token.kind == 'identifier' and next_token.value == 'FROM') \
                    or (
                    next_token.kind != 'column_or_alias' and next_token.kind != 'table_or_alias'
                    and next_token.kind != 'syntax'
                    and next_token.kind != 'value'):
                token.prev.next = next_token
                next_token.prev = token.prev
        else:
            pass

        token.str_value = token.value
        return token

    @staticmethod
    def _token_kind_value(token: Token) -> Token:
        token.str_value = token.value if token.value.isnumeric() else f'\'{token.value}\''
        return token

    @staticmethod
    def _token_kind_param(token: Token) -> Token:
        if token.next is None:
            raise ValueError(f'PARAM must be followed by parameter name')
        token.prev.next = token.next
        token.next.prev = token.prev
        token = token.next
        token.str_value = f'%({token.value})s'
        return token

    @staticmethod
    def _token_kind_unknown(token: Token):
        raise ValueError(f'Unknown kind {token.kind} for {token.value}')

    @staticmethod
    def _is_reserved_word(word: str) -> bool:
        return word in postgres_reserved_words or \
               word in postgres_unreserved_keywords or \
               word in postgres_reserved_can_use_as_func_or_type_words or \
               word in postgres_unreserved_cannot_use_as_func_or_type_words

    @staticmethod
    def map(result_set, column_names, clazz: Any) -> List[Table]:
        # first, let's get a dictionary of all table properties in clazz
        class_prop_lookup = {}
        o = clazz()
        for key in o.__dict__:
            if key.startswith('_') and isinstance(o.__dict__[key], PySqlTypeContainer):
                class_prop_lookup[key[1:]] = o.__dict__[key].py_to_sql

        # second, let's build a dictionary for each column name index
        column_name_lookup = {}
        for idx, column_name in enumerate(column_names):
            # skip any unknown columns
            if column_name not in class_prop_lookup:
                continue
            if column_name in column_name_lookup:
                raise ValueError(f'column {column_name} already exists')
            column_name_lookup[idx] = column_name

        # orm
        target: List[Table] = []
        for row in result_set:
            o = clazz()
            for idx, value in enumerate(row):
                # skip any unknown columns during row parsing
                if idx not in column_name_lookup:
                    continue
                column_name = column_name_lookup[idx]
                prop_type = class_prop_lookup[column_name]
                # check types
                if (prop_type.name.startswith('INT') and isinstance(value, int)) \
                        or (prop_type.name.startswith('STR') and isinstance(value, str)) \
                        or (prop_type.name.startswith('FLOAT') and isinstance(value, float)) \
                        or (prop_type.name.startswith('DATE_') and isinstance(value, datetime.date)) \
                        or (prop_type.name.startswith('TIME_') and isinstance(value, datetime.time)) \
                        or (prop_type.name.startswith('DATETIME') and isinstance(value, datetime.datetime)) \
                        or (prop_type.name.startswith('DICT') and isinstance(value, dict)) \
                        or (prop_type.name.startswith('BYTES') and isinstance(value, bytes)):
                    o.__dict__['_' + column_name].value = value
                else:
                    raise ValueError(f'type mismatch between {prop_type} and {value}')
            target.append(o)
        return target

    def __init__(self):
        self._sql: Optional[str] = None
        self._head = Token()
        self._tail = self._head

    def __getattr__(self, token: str) -> 'SQL':
        self._add_token(token, 'schema')
        return self

    def __call__(self, *args) -> 'SQL':
        if len(args) == 0:
            self._add_token(',', 'syntax')
            return self

        is_single_list_arg = len(args) == 1 and isinstance(args[0], list)
        is_single_set_arg = not is_single_list_arg and len(args) == 1 and isinstance(args[0], set)
        add_paren = is_single_list_arg or len(args) > 1 or isinstance(args[0], Callable)

        # a made-up convention for allowing raw values
        if is_single_set_arg:
            self._add_token(str(next(iter(args[0]))), 'syntax')
            return self

        if add_paren:
            self._add_token('(', 'syntax')
        for idx, arg in enumerate(args):
            if isinstance(arg, Callable):
                arg(self)
            else:
                value = ','.join([str(el) for el in arg]) if is_single_list_arg else str(arg)
                self._add_token(value, 'value')
            if add_paren and idx < len(args) - 1:
                self._add_token(',', 'syntax')
        if add_paren:
            self._add_token(')', 'syntax')
        return self

    def _add_token(self, raw_value: str, kind: str):
        # skip special syntax for subqueries
        if raw_value == '__':
            return

        tail = self._tail
        value = raw_value.lower()

        is_uppercase = raw_value.isupper() and len(raw_value) > 1
        is_capitalized = raw_value[0].isupper()
        is_reserved_word = SQL._is_reserved_word(value)

        if is_uppercase:
            if is_reserved_word:
                value = raw_value
                kind = 'identifier'
            elif value == 'param':
                kind = 'param'
            else:
                raise ValueError(f'{raw_value} is not a valid PostgreSQL identifier. Did you mean to uppercase it?')
        elif kind == 'schema':
            kind = 'table_or_alias' if is_capitalized else 'column_or_alias'
        else:
            pass

        token = Token(value=value, kind=kind, prev=tail, is_reserved=is_reserved_word, is_capitalized=is_capitalized,
                      is_uppercase=is_uppercase)

        tail.next = token
        self._tail = token

    def _parse(self):
        kind_lookup = {
            'identifier': SQL._token_kind_identifier,
            'table_or_alias': SQL._token_kind_table_or_alias,
            'column_or_alias': SQL._token_kind_column_or_alias,
            'syntax': SQL._token_kind_syntax,
            'value': SQL._token_kind_value,
            'param': SQL._token_kind_param,
        }

        token = self._head.next
        while token is not None:
            token = kind_lookup.get(token.kind, SQL._token_kind_unknown)(token)
            token = token.next

    def ready(self) -> str:
        if self._sql is None:
            self._parse()
            token = self._head.next
            str_values = []
            while token is not None:
                str_values.append(token.str_value)
                token = token.next
            self._sql = ' '.join(str_values)
        return self._sql
