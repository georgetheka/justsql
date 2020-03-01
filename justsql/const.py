from enum import Enum

postgres_reserved_words = (
    'all'
    'analyse'
    'analyze'
    'and'
    'any'
    'array'
    'as'
    'asc'
    'asymmetric'
    'both'
    'case'
    'cast'
    'check'
    'collate'
    'column'
    'constraint'
    'create'
    'current_catalog'
    'current_date'
    'current_role'
    'current_time'
    'current_timestamp'
    'current_user'
    'default'
    'deferrable'
    'desc'
    'distinct'
    'do'
    'else'
    'end'
    'except'
    'false'
    'fetch'
    'for'
    'foreign'
    'from'
    'grant'
    'group'
    'having'
    'in'
    'initially'
    'intersect'
    'into'
    'lateral'
    'leading'
    'limit'
    'localtime'
    'localtimestamp'
    'not'
    'null'
    'offset'
    'on'
    'only'
    'or'
    'order'
    'placing'
    'primary'
    'references'
    'returning'
    'select'
    'session_user'
    'some'
    'symmetric'
    'table'
    'then'
    'to'
    'trailing'
    'true'
    'union'
    'unique'
    'user'
    'using'
    'variadic'
    'when'
    'where'
    'window'
    'with'
)

postgres_unreserved_keywords = (
    'abort'
    'absolute'
    'access'
    'action'
    'add'
    'admin'
    'after'
    'aggregate'
    'also'
    'alter'
    'always'
    'assertion'
    'assignment'
    'at'
    'attach'
    'attribute'
    'backward'
    'before'
    'begin'
    'by'
    'cache'
    'called'
    'cascade'
    'cascaded'
    'catalog'
    'chain'
    'characteristics'
    'checkpoint'
    'class'
    'close'
    'cluster'
    'columns'
    'comment'
    'comments'
    'commit'
    'committed'
    'configuration'
    'conflict'
    'connection'
    'constraints'
    'content'
    'continue'
    'conversion'
    'copy'
    'cost'
    'csv'
    'cube'
    'current'
    'cursor'
    'cycle'
    'data'
    'database'
    'day'
    'deallocate'
    'declare'
    'defaults'
    'deferred'
    'definer'
    'delete'
    'delimiter'
    'delimiters'
    'depends'
    'detach'
    'dictionary'
    'disable'
    'discard'
    'document'
    'domain'
    'double'
    'drop'
    'each'
    'enable'
    'encoding'
    'encrypted'
    'enum'
    'escape'
    'event'
    'exclude'
    'excluding'
    'exclusive'
    'execute'
    'explain'
    'extension'
    'external'
    'family'
    'filter'
    'first'
    'following'
    'force'
    'forward'
    'function'
    'functions'
    'generated'
    'global'
    'granted'
    'handler'
    'header'
    'hold'
    'hour'
    'identity'
    'if'
    'immediate'
    'immutable'
    'implicit'
    'import'
    'including'
    'increment'
    'index'
    'indexes'
    'inherit'
    'inherits'
    'inline'
    'input'
    'insensitive'
    'insert'
    'instead'
    'invoker'
    'isolation'
    'key'
    'label'
    'language'
    'large'
    'last'
    'leakproof'
    'level'
    'listen'
    'load'
    'local'
    'location'
    'lock'
    'locked'
    'logged'
    'mapping'
    'match'
    'materialized'
    'maxvalue'
    'method'
    'minute'
    'minvalue'
    'mode'
    'month'
    'move'
    'name'
    'names'
    'new'
    'next'
    'no'
    'nothing'
    'notify'
    'nowait'
    'nulls'
    'object'
    'of'
    'off'
    'oids'
    'old'
    'operator'
    'option'
    'options'
    'ordinality'
    'over'
    'overriding'
    'owned'
    'owner'
    'parallel'
    'parser'
    'partial'
    'partition'
    'passing'
    'password'
    'plans'
    'policy'
    'preceding'
    'prepare'
    'prepared'
    'preserve'
    'prior'
    'privileges'
    'procedural'
    'procedure'
    'program'
    'publication'
    'quote'
    'range'
    'read'
    'reassign'
    'recheck'
    'recursive'
    'ref'
    'referencing'
    'refresh'
    'reindex'
    'relative'
    'release'
    'rename'
    'repeatable'
    'replace'
    'replica'
    'reset'
    'restart'
    'restrict'
    'returns'
    'revoke'
    'role'
    'rollback'
    'rollup'
    'rows'
    'rule'
    'savepoint'
    'schema'
    'schemas'
    'scroll'
    'search'
    'second'
    'security'
    'sequence'
    'sequences'
    'serializable'
    'server'
    'session'
    'set'
    'sets'
    'share'
    'show'
    'simple'
    'skip'
    'snapshot'
    'sql'
    'stable'
    'standalone'
    'start'
    'statement'
    'statistics'
    'stdin'
    'stdout'
    'storage'
    'strict'
    'strip'
    'subscription'
    'sysid'
    'system'
    'tables'
    'tablespace'
    'temp'
    'template'
    'temporary'
    'text'
    'transaction'
    'transform'
    'trigger'
    'truncate'
    'trusted'
    'type'
    'types'
    'unbounded'
    'uncommitted'
    'unencrypted'
    'unknown'
    'unlisten'
    'unlogged'
    'until'
    'update'
    'vacuum'
    'valid'
    'validate'
    'validator'
    'value'
    'varying'
    'version'
    'view'
    'views'
    'volatile'
    'whitespace'
    'within'
    'without'
    'work'
    'wrapper'
    'write'
    'xml'
    'year'
    'yes'
    'zone'
)

postgres_unreserved_cannot_use_as_func_or_type_words = (
    'between'
    'bigint'
    'bit'
    'boolean'
    'char'
    'character'
    'coalesce'
    'dec'
    'decimal'
    'exists'
    'extract'
    'float'
    'greatest'
    'grouping'
    'inout'
    'int'
    'integer'
    'interval'
    'least'
    'national'
    'nchar'
    'none'
    'nullif'
    'numeric'
    'out'
    'overlay'
    'position'
    'precision'
    'real'
    'row'
    'setof'
    'smallint'
    'substring'
    'time'
    'timestamp'
    'treat'
    'trim'
    'values'
    'varchar'
    'xmlattributes'
    'xmlconcat'
    'xmlelement'
    'xmlexists'
    'xmlforest'
    'xmlnamespaces'
    'xmlparse'
    'xmlpi'
    'xmlroot'
    'xmlserialize'
    'xmltable'
)

postgres_reserved_can_use_as_func_or_type_words = (
    'authorization'
    'binary'
    'collation'
    'concurrently'
    'cross'
    'current_schema'
    'freeze'
    'full'
    'ilike'
    'inner'
    'is'
    'isnull'
    'join'
    'left'
    'like'
    'natural'
    'notnull'
    'outer'
    'overlaps'
    'right'
    'similar'
    'tablesample'
    'verbose'
)


# todo: document
class PyToSqlType(Enum):
    INT_SMALLINT = 1
    INT_INT = 2
    INT_BIGINT = 3
    INT_SMALLSERIAL = 4
    INT_SERIAL = 5
    INT_BIGSERIAL = 6

    FLOAT_NUMERIC = 7
    BOOL_BOOLEAN = 8

    DATE_DATE = 9
    TIME_TIME = 10
    DATETIME_DATETIME = 11
    DATETIME_TIMESTAMP = 12
    DATETIME_TIMESTAMPZ = 13
    STR_INTERVAL = 14

    STR_UUID = 15
    DICT_JSONB = 16  # not handled

    BYTES_BYTEA = 17  # not handled

    STR_TEXT = 18
    STR_CHAR_1 = 19
    STR_CHAR_2 = 20
    STR_CHAR_3 = 21
    STR_CHAR_4 = 22
    STR_CHAR_5 = 23
    STR_CHAR_6 = 24
    STR_CHAR_7 = 25
    STR_CHAR_8 = 26
    STR_CHAR_9 = 27
    STR_CHAR_10 = 28
    STR_CHAR_11 = 29
    STR_CHAR_12 = 30
    STR_CHAR_13 = 31
    STR_CHAR_14 = 32
    STR_CHAR_15 = 33
    STR_CHAR_16 = 34
    STR_CHAR_17 = 35
    STR_CHAR_18 = 36
    STR_CHAR_19 = 37
    STR_CHAR_20 = 38
    STR_CHAR_21 = 39
    STR_CHAR_22 = 40
    STR_CHAR_23 = 41
    STR_CHAR_24 = 42
    STR_CHAR_25 = 43
    STR_CHAR_26 = 44
    STR_CHAR_27 = 45
    STR_CHAR_28 = 46
    STR_CHAR_29 = 47
    STR_CHAR_30 = 48
    STR_CHAR_31 = 49
    STR_CHAR_32 = 50
    STR_CHAR_33 = 51
    STR_CHAR_34 = 52
    STR_CHAR_35 = 53
    STR_CHAR_36 = 54
    STR_CHAR_37 = 55
    STR_CHAR_38 = 56
    STR_CHAR_39 = 57
    STR_CHAR_40 = 58
    STR_CHAR_41 = 59
    STR_CHAR_42 = 60
    STR_CHAR_43 = 61
    STR_CHAR_44 = 62
    STR_CHAR_45 = 63
    STR_CHAR_46 = 64
    STR_CHAR_47 = 65
    STR_CHAR_48 = 66
    STR_CHAR_49 = 67
    STR_CHAR_50 = 68
    STR_CHAR_51 = 69
    STR_CHAR_52 = 70
    STR_CHAR_53 = 71
    STR_CHAR_54 = 72
    STR_CHAR_55 = 73
    STR_CHAR_56 = 74
    STR_CHAR_57 = 75
    STR_CHAR_58 = 76
    STR_CHAR_59 = 77
    STR_CHAR_60 = 78
    STR_CHAR_61 = 79
    STR_CHAR_62 = 80
    STR_CHAR_63 = 81
    STR_CHAR_64 = 82
    STR_CHAR_65 = 83
    STR_CHAR_66 = 84
    STR_CHAR_67 = 85
    STR_CHAR_68 = 86
    STR_CHAR_69 = 87
    STR_CHAR_70 = 88
    STR_CHAR_71 = 89
    STR_CHAR_72 = 90
    STR_CHAR_73 = 91
    STR_CHAR_74 = 92
    STR_CHAR_75 = 93
    STR_CHAR_76 = 94
    STR_CHAR_77 = 95
    STR_CHAR_78 = 96
    STR_CHAR_79 = 97
    STR_CHAR_80 = 98
    STR_CHAR_81 = 99
    STR_CHAR_82 = 100
    STR_CHAR_83 = 101
    STR_CHAR_84 = 102
    STR_CHAR_85 = 103
    STR_CHAR_86 = 104
    STR_CHAR_87 = 105
    STR_CHAR_88 = 106
    STR_CHAR_89 = 107
    STR_CHAR_90 = 108
    STR_CHAR_91 = 109
    STR_CHAR_92 = 110
    STR_CHAR_93 = 111
    STR_CHAR_94 = 112
    STR_CHAR_95 = 113
    STR_CHAR_96 = 114
    STR_CHAR_97 = 115
    STR_CHAR_98 = 116
    STR_CHAR_99 = 117
    STR_CHAR_100 = 118
    STR_CHAR_101 = 119
    STR_CHAR_102 = 120
    STR_CHAR_103 = 121
    STR_CHAR_104 = 122
    STR_CHAR_105 = 123
    STR_CHAR_106 = 124
    STR_CHAR_107 = 125
    STR_CHAR_108 = 126
    STR_CHAR_109 = 127
    STR_CHAR_110 = 128
    STR_CHAR_256 = 274
    STR_CHAR_512 = 275
    STR_CHAR_1024 = 276
    STR_CHAR_2048 = 277
    STR_CHAR_4096 = 278
    STR_CHAR_8192 = 279
    STR_CHAR_16384 = 280
    STR_CHAR_32768 = 281
    STR_CHAR_65536 = 282
    STR_CHAR_131072 = 283
    STR_CHAR_262144 = 284
    STR_CHAR_524288 = 285

    STR_VARCHAR_1 = 286
    STR_VARCHAR_2 = 287
    STR_VARCHAR_3 = 288
    STR_VARCHAR_4 = 289
    STR_VARCHAR_5 = 290
    STR_VARCHAR_6 = 291
    STR_VARCHAR_7 = 292
    STR_VARCHAR_8 = 293
    STR_VARCHAR_9 = 294
    STR_VARCHAR_10 = 295
    STR_VARCHAR_11 = 296
    STR_VARCHAR_12 = 297
    STR_VARCHAR_13 = 298
    STR_VARCHAR_14 = 299
    STR_VARCHAR_15 = 300
    STR_VARCHAR_16 = 301
    STR_VARCHAR_17 = 302
    STR_VARCHAR_18 = 303
    STR_VARCHAR_19 = 304
    STR_VARCHAR_20 = 305
    STR_VARCHAR_21 = 306
    STR_VARCHAR_22 = 307
    STR_VARCHAR_23 = 308
    STR_VARCHAR_24 = 309
    STR_VARCHAR_25 = 310
    STR_VARCHAR_26 = 311
    STR_VARCHAR_27 = 312
    STR_VARCHAR_28 = 313
    STR_VARCHAR_29 = 314
    STR_VARCHAR_30 = 315
    STR_VARCHAR_31 = 316
    STR_VARCHAR_32 = 317
    STR_VARCHAR_33 = 318
    STR_VARCHAR_34 = 319
    STR_VARCHAR_35 = 320
    STR_VARCHAR_36 = 321
    STR_VARCHAR_37 = 322
    STR_VARCHAR_38 = 323
    STR_VARCHAR_39 = 324
    STR_VARCHAR_40 = 325
    STR_VARCHAR_41 = 326
    STR_VARCHAR_42 = 327
    STR_VARCHAR_43 = 328
    STR_VARCHAR_44 = 329
    STR_VARCHAR_45 = 330
    STR_VARCHAR_46 = 331
    STR_VARCHAR_47 = 332
    STR_VARCHAR_48 = 333
    STR_VARCHAR_49 = 334
    STR_VARCHAR_50 = 335
    STR_VARCHAR_51 = 336
    STR_VARCHAR_52 = 337
    STR_VARCHAR_53 = 338
    STR_VARCHAR_54 = 339
    STR_VARCHAR_55 = 340
    STR_VARCHAR_56 = 341
    STR_VARCHAR_57 = 342
    STR_VARCHAR_58 = 343
    STR_VARCHAR_59 = 344
    STR_VARCHAR_60 = 345
    STR_VARCHAR_61 = 346
    STR_VARCHAR_62 = 347
    STR_VARCHAR_63 = 348
    STR_VARCHAR_64 = 349
    STR_VARCHAR_65 = 350
    STR_VARCHAR_66 = 351
    STR_VARCHAR_67 = 352
    STR_VARCHAR_68 = 353
    STR_VARCHAR_69 = 354
    STR_VARCHAR_70 = 355
    STR_VARCHAR_71 = 356
    STR_VARCHAR_72 = 357
    STR_VARCHAR_73 = 358
    STR_VARCHAR_74 = 359
    STR_VARCHAR_75 = 360
    STR_VARCHAR_76 = 361
    STR_VARCHAR_77 = 362
    STR_VARCHAR_78 = 363
    STR_VARCHAR_79 = 364
    STR_VARCHAR_80 = 365
    STR_VARCHAR_81 = 366
    STR_VARCHAR_82 = 367
    STR_VARCHAR_83 = 368
    STR_VARCHAR_84 = 369
    STR_VARCHAR_85 = 370
    STR_VARCHAR_86 = 371
    STR_VARCHAR_87 = 372
    STR_VARCHAR_88 = 373
    STR_VARCHAR_89 = 374
    STR_VARCHAR_90 = 375
    STR_VARCHAR_91 = 376
    STR_VARCHAR_92 = 377
    STR_VARCHAR_93 = 378
    STR_VARCHAR_94 = 379
    STR_VARCHAR_95 = 380
    STR_VARCHAR_96 = 381
    STR_VARCHAR_97 = 382
    STR_VARCHAR_98 = 383
    STR_VARCHAR_99 = 384
    STR_VARCHAR_100 = 385
    STR_VARCHAR_101 = 386
    STR_VARCHAR_102 = 387
    STR_VARCHAR_103 = 388
    STR_VARCHAR_104 = 389
    STR_VARCHAR_105 = 390
    STR_VARCHAR_106 = 391
    STR_VARCHAR_107 = 392
    STR_VARCHAR_108 = 393
    STR_VARCHAR_109 = 394
    STR_VARCHAR_110 = 395
    STR_VARCHAR_111 = 396
    STR_VARCHAR_112 = 397
    STR_VARCHAR_113 = 398
    STR_VARCHAR_114 = 399
    STR_VARCHAR_115 = 400
    STR_VARCHAR_116 = 401
    STR_VARCHAR_117 = 402
    STR_VARCHAR_118 = 403
    STR_VARCHAR_119 = 404
    STR_VARCHAR_120 = 405
    STR_VARCHAR_121 = 406
    STR_VARCHAR_122 = 407
    STR_VARCHAR_123 = 408
    STR_VARCHAR_124 = 409
    STR_VARCHAR_125 = 410
    STR_VARCHAR_126 = 411
    STR_VARCHAR_127 = 412
    STR_VARCHAR_128 = 413
    STR_VARCHAR_256 = 541
    STR_VARCHAR_512 = 542
    STR_VARCHAR_1024 = 543
    STR_VARCHAR_2048 = 544
    STR_VARCHAR_4096 = 545
    STR_VARCHAR_8192 = 546
    STR_VARCHAR_16384 = 547
    STR_VARCHAR_32768 = 548
    STR_VARCHAR_65536 = 549
    STR_VARCHAR_131072 = 550
    STR_VARCHAR_262144 = 551
    STR_VARCHAR_524288 = 552
