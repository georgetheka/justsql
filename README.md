# JustSQL 

Direct SQL expressions in Python and simple query mapping.

### What?
A library that aims to write transparent and 1-1 SQL mapping as well as auto object-to-table mapping within the syntactical confines of python.

### Can I use this in production? 
You shouldn't. The current version is nothing more than a PoC built on Sunday afternoon.

## Roadmap
Current Version: 0.0.0-alpha: This version is pretty simple. It does not implement a SQL interpreter 
(this would not have been feasible within a few hours) and it does not build a syntax tree. It does simply tokenize
all intercepted attributes and function calls using a double linked list. It does a few smart things though such as 
automatically quote things when needed, add necessary periods and commas (even dealing with last commas) in SQL syntax.

Next Steps:
* V0: Documentation, complete test-coverage, optimizations for current versions
* V1: Build a proper SQL interpreter that can parse the language fully into an AST and enable validations for table, column aliases and so much more. 
* V2: Build a migration tool to pair it with.
* V3: Strong integration with postgres' JSONB type for document-centric schemas.


## Usage

```python
from justsql.sql import SQL

statement = SQL()
```

Convert it to a cached query-string

```python
#prepare SQL (string) and cache it
sql = statement.ready()

# establish a connection context, for example using psycopg2
result_set = cursor.execute(sql)
```

So what exactly can `SQL()` do? Let's start with an example of querying a field from a table
```python
query = SQL()
    .SELECT.name() \
    .FROM.Product \
    .WHERE.id.IN(1,2,3)
```

### Conventions

There are some strong conventions used in order to achieve the kind of expressivity that SQL requires within the constraints of python language.
However, once these conventions are followed, it should be easy, if not trivial, to see what the generated SQL is.

* UPPERCASE all PostgreSQL language identifiers (library will tell you if you uppercase a non-language identifier)
* Capitalize all tables or table aliases (any collisions with language identifiers will result in auto quoting)
* lowercase all columns, column aliases, or parameter names (any collisions with language identifiers will result in auto quoting)
* use () to as a comma (you can place a comma even after the last item in a select field-set, group-by, or order-by field-set)
* use (['value']) if you want to express a single item as a SQL list (i.e. `SELECT (1)` -> `SELECT([1])`)
* use ({'value'}) if you want to define raw strings or various operators (i.e. `id = 3` -> `id({'='})(3)`)
* use `PARAM.param_name` to parametrize using DB-API 2.0 format
* lambdas are used for sub-queries or table field definitions

This will make more sense with a more thorough example.

## An Example 

Let's say we want to store the instruments of a musical score in a postgres database.
We will create the following tables: `score`, `instrument`.

1. Create table `score`
```python
    drop_table = SQL().DROP.TABLE.IF.EXISTS.Score.CASCADE
    create_table = SQL().CREATE.TABLE.Score(
        lambda s: s.id.SERIAL.PRIMARY.KEY,
        lambda s: s.name.VARCHAR([16]).NOT.NULL,
        lambda s: s.desc.VARCHAR([128]),
        lambda s: s.date_created.TIMESTAMP.WITHOUT.TIME.ZONE.NOT.NULL.DEFAULT.CURRENT_TIMESTAMP,
        lambda s: s.date_updated.TIMESTAMP.WITHOUT.TIME.ZONE.NOT.NULL.DEFAULT.CURRENT_TIMESTAMP,
        lambda s: s.date_deleted.TIMESTAMP.WITHOUT.TIME.ZONE
    )
    conn.execute(drop_table.ready())
    conn.execute(create_table.ready())
```

2. Insert a `score`
```python
    insert_rows = SQL().INSERT.INTO.Score(
        lambda s: s.name,
        lambda s: s.desc,
    ).VALUES('Electro-Overture', 'MIDI Orchestral Score')
    conn.execute(insert_rows.ready())
```

3. Create table `instrument`
```python
    drop_table = SQL().DROP.TABLE.IF.EXISTS.Instrument.CASCADE
    create_table = SQL().CREATE.TABLE.Instrument(
        lambda s: s.id.SERIAL.PRIMARY.KEY,
        lambda s: s.score_id.INT.NOT.NULL.REFERENCES.Score(lambda s: s.id),
        lambda s: s.name.VARCHAR([16]).NOT.NULL,
        lambda s: s.desc.VARCHAR([128]),
        lambda s: s.date_created.TIMESTAMP.WITHOUT.TIME.ZONE.NOT.NULL.DEFAULT.CURRENT_TIMESTAMP,
        lambda s: s.date_updated.TIMESTAMP.WITHOUT.TIME.ZONE.NOT.NULL.DEFAULT.CURRENT_TIMESTAMP,
        lambda s: s.date_deleted.TIMESTAMP.WITHOUT.TIME.ZONE
    )
    conn.execute(drop_table.ready())
    conn.execute(create_table.ready())
```
4. Insert three `instruments`
```python
    insert_rows_1 = SQL().INSERT.INTO.Instrument(
        lambda s: s.score_id,
        lambda s: s.name,
        lambda s: s.desc,
    ).VALUES(1, 'digital guitar', 'midi guitar signal')
    insert_rows_2 = SQL().INSERT.INTO.Instrument(
        lambda s: s.score_id,
        lambda s: s.name,
        lambda s: s.desc,
    ).VALUES(1, 'digital drums', 'drumkit midi signal')
    insert_rows_3 = SQL().INSERT.INTO.Instrument(
        lambda s: s.score_id,
        lambda s: s.name,
        lambda s: s.desc,
    ).VALUES(1, 'bass', 'midi bass signal')
    conn.execute(insert_rows_1.ready())
    conn.execute(insert_rows_2.ready())
    conn.execute(insert_rows_3.ready())
```

5. Define Python objects by inheriting from `Table`. Use the `_make()` method
to define property-like fields that actually perform bidirectional binding and `PyToSqlType` enum
to define the mapping type for each field. 

```python
from poohpy.const import PyToSqLType
from poohpy.sql import Table

class Score(Table):
    def __init__(self):
        self._make('id', PyToSqlType.INT_SERIAL)
        self._make('name', PyToSqlType.STR_VARCHAR_16)
        self._make('desc', PyToSqlType.STR_VARCHAR_256)
        self._make('date_created', PyToSqlType.DATETIME_TIMESTAMP)
        self._make('date_updated', PyToSqlType.DATETIME_TIMESTAMP)
        self._make('date_deleted', PyToSqlType.DATETIME_TIMESTAMP)


class Instrument(Table):
    def __init__(self):
        self._make('id', PyToSqlType.INT_SERIAL)
        self._make('score_id', PyToSqlType.INT_INT)
        self._make('name', PyToSqlType.STR_VARCHAR_16)
        self._make('desc', PyToSqlType.STR_VARCHAR_256)
        self._make('date_created', PyToSqlType.DATETIME_TIMESTAMP)
        self._make('date_updated', PyToSqlType.DATETIME_TIMESTAMP)
        self._make('date_deleted', PyToSqlType.DATETIME_TIMESTAMP)
```

6. Run a simple query:
```python
    query = SQL() \
        .SELECT \
            .Sc.id() \
            .Sc.name() \
            .Sc.desc() \
            .Sc.date_created() \
            .Sc.date_updated() \
        .FROM.Score.AS.Sc \
        .WHERE.date_deleted.IS.NULL \
        .AND.id({'='})(1)

    # this method happens to return a list of column names
    # from a cursor descriptor, implementation does not matter
    result_set, column_names = _exec_statement(query.ready())

    # a list of objects with type Score
    list_of_scores = SQL.map(result_set, column_names, Score)
``` 

7. A more contrived query example showing use of joins, aliases ans subqueries in select field-set
```python
    query = SQL() \
        .SELECT \
            .Instrument.id() \
            .Instrument.name() \
            .Instrument.desc() \
            .Instrument.date_created() \
            .Instrument.date_updated() \
            .__(lambda s: s.SELECT
                .S.name
                .FROM.Score.AS.S
                .WHERE.S.id({'='}).Instrument.score_id) \
            .AS.score_name() \
        .FROM.Score \
        .LEFT.OUTER.JOIN.Instrument.ON.Instrument.score_id({'='}).Score.id \
        .WHERE.Score.date_deleted.IS.NULL \
        .AND.Instrument.date_deleted.IS.NULL \
        .AND.Score.id.IN(1, 2, 3, 4)

    result_set, column_names = _exec_statement(query.ready())

    list_of_instruments = SQL.map(result_set, column_names, Instrument)
```

See tests for full code examples.
 
 


