CX_ROOT
  |
  |--CX_STATIC_CHECK
  |
  |--CX_DYNAMIC_CHECK
  |    |
  |    |--CX_SY_ARITHMETIC_ERROR
  |    |    |
  |    |    |--CX_SY_ZERODIVIDE
  |    |    |
  |    |    |--CX_SY_ARITHMETIC_OVERFLOW
  |    |    |
  |    |    |--CX_SY_ARG_OUT_OF_DOMAIN
  |    |    |
  |    |    |--CX_SY_PRECISION_LOSS
  |    |
  |    |--CX_SY_ASSIGN_ERROR
  |    |    |
  |    |    |--CX_SY_ASSIGN_CAST_ERROR
  |    |    |    |
  |    |    |    |--CX_SY_ASSIGN_CAST_ILLEGAL_CAST
  |    |    |    |
  |    |    |    |--CX_SY_ASSIGN_CAST_UNKNOWN_TYPE
  |    |    |
  |    |    |--CX_SY_ASSIGN_OUT_OF_RANGE
  |    |
  |    |--CX_SY_CODEPAGE_CONVERTER_INIT
  |    |
  |    |--CX_SY_CONVERSION_ERROR
  |    |    |
  |    |    |--CX_SY_CONVERSION_CODEPAGE
  |    |    |
  |    |    |--CX_SY_CONVERSION_CODEPAGE_EX
  |    |    |
  |    |    |--CX_SY_CONVERSION_DATA_LOSS
  |    |    |
  |    |    |--CX_SY_CONVERSION_EXACT_NOT_SUP
  |    |    |
  |    |    |--CX_SY_CONVERSION_INEXACT_FLTP
  |    |    |
  |    |    |--CX_SY_CONVERSION_NO_BOOLEAN
  |    |    |
  |    |    |--CX_SY_CONVERSION_NO_DATE_TIME
  |    |    |
  |    |    |--CX_SY_CONVERSION_NO_ENUM_VALUE
  |    |    |
  |    |    |--CX_SY_CONVERSION_NO_NUMBER
  |    |    |
  |    |    |--CX_SY_CONVERSION_NO_QNAME
  |    |    |
  |    |    |--CX_SY_CONVERSION_NO_RAW
  |    |    |
  |    |    |--CX_SY_CONVERSION_NO_TIME
  |    |    |
  |    |    |--CX_SY_CONVERSION_NO_UUID
  |    |    |
  |    |    |--CX_SY_CONVERSION_OVERFLOW
  |    |    |
  |    |    |--CX_SY_CONVERSION_ROUNDING
  |    |    |
  |    |    |--CX_SY_CONVERSION_SRC_TOO_SHORT
  |    |    |
  |    |    |--CX_SY_CONVERSION_UNKNOWN_LANGU
  |    |
  |    |--CX_SY_CREATE_ERROR
  |    |    |
  |    |    |--CX_SY_CREATE_OBJECT_ERROR
  |    |    |
  |    |    |--CX_SY_CREATE_DATA_ERROR
  |    |
  |    |--CX_SY_DATA_ACCESS_ERROR
  |    |    |
  |    |    |--CX_SY_RANGE_OUT_OF_BOUNDS
  |    |    |
  |    |    |--CX_SY_TAB_RANGE_OUT_OF_BOUNDS
  |    |    |
  |    |    |--CX_SY_OFFSET_NOT_ALLOWED
  |    |
  |    |--CX_SY_DB_PROCEDURE_CALL
  |    |    |
  |    |    |--CX_SY_DB_PROCEDURE_CONNECTION
  |    |    |
  |    |    |--CX_SY_DB_PROCEDURE_NOT_FOUND
  |    |    |
  |    |    |--CX_SY_DB_PROCEDURE_NOT_SUPP
  |    |    |
  |    |    |--CX_SY_DB_PROCEDURE_OVERFLOW
  |    |    |
  |    |    |--CX_SY_DB_PROCEDURE_PARAMETER
  |    |         |
  |    |         |--CX_SY_DB_PROCEDURE_DYN_IN_OUT
  |    |         |
  |    |         |--CX_SY_DB_PROCEDURE_DYN_MISSING
  |    |         |
  |    |         |--CX_SY_DB_PROCEDURE_DYN_NOT_FND
  |    |         |
  |    |         |--CX_SY_DB_PROCEDURE_TYPE_ERROR
  |    |
  |    |--CX_SY_DYN_CALL_ERROR
  |    |    |
  |    |    |--CX_SY_DYN_CALL_ILLEGAL_CLASS
  |    |    |
  |    |    |--CX_SY_DYN_CALL_ILLEGAL_FORM
  |    |    |
  |    |    |--CX_SY_DYN_CALL_ILLEGAL_FUNC
  |    |    |
  |    |    |--CX_SY_DYN_CALL_ILLEGAL_METHOD
  |    |    |
  |    |    |--CX_SY_DYN_CALL_PARAMETER_ERROR
  |    |         |
  |    |         |--CX_SY_DYN_CALL_EXCP_NOT_FOUND
  |    |         |
  |    |         |--CX_SY_DYN_CALL_ILLEGAL_TYPE
  |    |         |
  |    |         |--CX_SY_DYN_CALL_PARAM_MISSING
  |    |         |
  |    |         |--CX_SY_DYN_CALL_PARAM_NOT_FOUND
  |    |
  |    |--CX_SY_EXPORT_NO_SHARED_MEMORY
  |    |
  |    |--CX_SY_FILE_ACCESS_ERROR
  |    |    |
  |    |    |--CX_SY_FILE_AUTHORITY
  |    |    |
  |    |    |--CX_SY_FILE_CLOSE
  |    |    |
  |    |    |--CX_SY_FILE_IO
  |    |    |
  |    |    |--CX_SY_FILE_OPEN
  |    |    |
  |    |    |--CX_SY_FILE_OPEN_MODE
  |    |    |
  |    |    |--CX_SY_FILE_POSITION
  |    |    |
  |    |    |--CX_SY_FILE_TRUNCATE
  |    |
  |    |--CX_SY_FIND_INFINITE_LOOP
  |    |
  |    |--CX_SY_GEN_SOURCE_TOO_WIDE
  |    |
  |    |--CX_SY_IMPORT_MISMATCH_ERROR
  |    |    |
  |    |    |--CX_SY_IMPORT_FORMAT_ERROR
  |    |
  |    |--CX_SY_ITAB_ERROR
  |    |    |
  |    |    |--CX_SY_ITAB_DUPLICATE_KEY
  |    |    |
  |    |    |--CX_SY_ITAB_DYN_LOOP
  |    |    |
  |    |    |--CX_SY_ITAB_LINE_NOT_FOUND
  |    |
  |    |--CX_SY_MATCHER
  |    |    |
  |    |    |--CX_SY_REGEX_TOO_COMPLEX
  |    |    |
  |    |    |--CX_SY_INVALID_REGEX_FORMAT
  |    |
  |    |--CX_SY_MOVE_CAST_ERROR
  |    |
  |    |--CX_SY_PROGRAM_NOT_FOUND
  |    |
  |    |--CX_SY_PROVIDE_EXCEPTION
  |    |    |
  |    |    |--CX_SY_PROVIDE_INTERVAL_OVERLAP
  |    |    |
  |    |    |--CX_SY_PROVIDE_TABLE_NOT_SORTED
  |    |
  |    |--CX_SY_READ_SRC_LINE_TOO_LONG
  |    |
  |    |--CX_SY_REF_IS_INITIAL
  |    |
  |    |--CX_SY_REGEX
  |    |    |
  |    |    |--CX_SY_INVALID_REGEX
  |    |
  |    |--CX_SY_REPLACE_INFINITE_LOOP
  |    |
  |    |--CX_SY_SCAN_SOURCE_TOO_WIDE
  |    |
  |    |--CX_SY_SQL_ERROR
  |    |    |
  |    |    |--CX_SY_DB_PROCEDURE
  |    |    |    |
  |    |    |    |--CX_SY_DB_PROCEDURE_SQL_ERROR
  |    |    |
  |    |    |--CX_SY_EXPIMP_DB_SQL_ERROR
  |    |    |
  |    |    |--CX_SY_OPEN_SQL_ERROR
  |    |    |    |
  |    |    |    |--CX_SY_OPEN_SQL_DB
  |    |    |    |
  |    |    |    |--CX_SY_DYNAMIC_OSQL_ERROR
  |    |    |    |    |
  |    |    |    |    |--CX_SY_DYNAMIC_OSQL_SEMANTICS
  |    |    |    |    |
  |    |    |    |    |--CX_SY_DYNAMIC_OSQL_SYNTAX
  |    |    |    |
  |    |    |    |--CX_SY_SQL_UNSUPPORTED_FEATURE
  |    |    |
  |    |    |--CX_SY_NATIVE_SQL_ERROR
  |    |
  |    |--CX_SY_UNKNOWN_CURRENCY
  |    |
  |    |--CX_SY_WRITE_INVALID_STYLE
  |    |
  |    |--CX_SY_WRITE_SRC_LINE_TOO_LONG
  |    |
  |    |--CX_TRANSFORMATION_ERROR
  |         |
  |         |--CX_ST_ERROR
  |         |    |
  |         |    |--CX_ST_CALL_ERROR
  |         |    |
  |         |    |--CX_ST_CALL_METHOD_ERROR
  |         |    |
  |         |    |--CX_ST_CONSTRAINT_ERROR
  |         |    |
  |         |    |--CX_ST_CONDITION
  |         |    |
  |         |    |--CX_ST_DESERIALIZATION_ERROR
  |         |    |
  |         |    |--CX_ST_FORMAT_ERROR
  |         |    |
  |         |    |--CX_ST_INVALID_XML
  |         |    |
  |         |    |--CX_ST_MATCH
  |         |    |
  |         |    |--CX_ST_REF_ACCESS
  |         |    |
  |         |    |--CX_ST_RUNTIME_ERROR
  |         |    |
  |         |    |--CX_ST_SERIALIZATION_ERROR
  |         |
  |         |--CX_XSLT_EXCEPTION
  |              |
  |              |--CX_XSLT_SYSTEM_ERROR
  |                   |
  |                   |--CX_XSLT_ABAP_CALL_ERROR
  |                   |
  |                   |--CX_XSLT_DESERIALIZATION_ERROR
  |                   |
  |                   |--CX_XSLT_FORMAT_ERROR
  |                   |
  |                   |--CX_XSLT_RUNTIME_ERROR
  |                   |
  |                   |--CX_XSLT_SERIALIZATION_ERROR
  |         |
  |         |--CX_SY_TRANS_OPTION_ERROR
  |
  |--CX_NO_CHECK
       |
       |--CX_BADI
       |    |
       |    |--CX_BADI_CONTEXT_ERROR
       |    |
       |    |--CX_BADI_FILTER_ERROR
       |    |
       |    |--CX_BADI_INITIAL_CONTEXT
       |    |
       |    |--CX_BADI_INITIAL_REFERENCE
       |    |
       |    |--CX_BADI_NOT_SINGLE_USE
       |    |    |
       |    |    |--CX_BADI_MULTIPLY_IMPLEMENTED
       |    |    |
       |    |    |--CX_BADI_NOT_IMPLEMENTED
       |    |
       |    |--CX_BADI_UNKNOWN_ERROR
       |
       |--CX_SY_AUTHORIZATION_ERROR
       |
       |--CX_SY_EXPORT_BUFFER_NO_MEMORY
       |
       |--CX_SY_GENERATE_SUBPOOL_FULL
       |
       |--CX_SY_ILLEGAL_HANDLER
       |
       |--CX_SY_LOCALIZATION_ERROR
       |
       |--CX_SY_NESTED_PRINT_ON
       |
       |--CX_SY_NO_HANDLER
       |
       |--CX_SY_PIPES_NOT_SUPPORTED
       |
       |--CX_SY_PIPE_REOPEN
       |
       |--CX_SY_REMOTE_CALL_ERROR
       |    |
       |    |--CX_SY_RMC_COMM_FAILURE
       |    |
       |    |--CX_SY_RMC_INVALID_STATUS
       |    |
       |    |--CX_SY_RMC_SYSTEM_FAILURE
       |
       |--CX_SY_TOO_MANY_FILES



