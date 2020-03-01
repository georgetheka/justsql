from typing import List, Optional

import psycopg2 as psycopg2

from justsql.const import PyToSqlType
from justsql.sql import SQL, Table

# todo: move these to a settings file
# database configuration
_host = 'localhost'
_database = 'justsql'
_user = 'justsql'
_pass = 'justsql'


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


def _exec_statement(statement: str, write: bool = False) -> (Optional[List], List[str]):
    conn = None
    column_names = []
    try:
        conn = psycopg2.connect(host=_host, database=_database, user=_user, password=_pass)
        cur = conn.cursor()
        cur.execute(statement)
        if write:
            cur.close()
            conn.commit()
            result_set = None
        else:
            result_set = cur.fetchall()
            column_names = [desc[0] for desc in cur.description]
            cur.close()
    except (Exception, psycopg2.DatabaseError) as error:
        raise error
    finally:
        if conn is not None:
            conn.close()
    return result_set, column_names


def setup_module(module):
    # create table score
    drop_table = SQL().DROP.TABLE.IF.EXISTS.Score.CASCADE
    create_table = SQL().CREATE.TABLE.Score(
        lambda s: s.id.SERIAL.PRIMARY.KEY,
        lambda s: s.name.VARCHAR([16]).NOT.NULL,
        lambda s: s.desc.VARCHAR([128]),
        lambda s: s.date_created.TIMESTAMP.WITHOUT.TIME.ZONE.NOT.NULL.DEFAULT.CURRENT_TIMESTAMP,
        lambda s: s.date_updated.TIMESTAMP.WITHOUT.TIME.ZONE.NOT.NULL.DEFAULT.CURRENT_TIMESTAMP,
        lambda s: s.date_deleted.TIMESTAMP.WITHOUT.TIME.ZONE
    )
    insert_rows = SQL().INSERT.INTO.Score(
        lambda s: s.name,
        lambda s: s.desc,
    ).VALUES('Electro-Overture', 'MIDI Orchestral Score')

    _exec_statement(drop_table.ready(), True)
    _exec_statement(create_table.ready(), True)
    _exec_statement(insert_rows.ready(), True)

    # create table instrument
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
    _exec_statement(drop_table.ready(), True)
    _exec_statement(create_table.ready(), True)
    _exec_statement(insert_rows_1.ready(), True)
    _exec_statement(insert_rows_2.ready(), True)
    _exec_statement(insert_rows_3.ready(), True)


def test_simple_query():
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

    result_set, column_names = _exec_statement(query.ready())

    mapped_result = SQL.map(result_set, column_names, Score)

    assert len(mapped_result) == 1
    score = mapped_result[0]

    assert score.id == 1
    assert score.name is not None
    assert score.desc is not None


def test_contrived_query():
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

    mapped_result = SQL.map(result_set, column_names, Instrument)

    assert len(mapped_result) > 1
