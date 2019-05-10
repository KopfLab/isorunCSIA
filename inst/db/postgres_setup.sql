# Data base setup

-- Database: irms-db1
-- Note: all timestamps must be WITH timezone, otherwise R has trouble

-- DROP DATABASE "irms-db1";

CREATE DATABASE "irms-db1"
    WITH
    OWNER = root
    ENCODING = 'UTF8'
    CONNECTION LIMIT = -1;

-- Table: instruments

DROP TABLE IF EXISTS instruments;

CREATE TABLE instruments
(
  id character varying(20) PRIMARY KEY,
  description character varying(100) NULL,
  location character varying(255) NULL
);

INSERT INTO instruments(id, description)
	VALUES ('TEST', 'Test Instrument');

SELECT * FROM instruments;

-- Table: instrument_logs

DROP TABLE IF EXISTS instrument_logs;

CREATE TABLE instrument_logs
(
  instrument_log_id SERIAL PRIMARY KEY,
  instrument_id character varying(20) NOT NULL references instruments(id),
  log_datetime timestamp with time zone,
  login character varying(20) NULL,
  mode character varying(20) NULL,
  category character varying(50) NULL,
  data_key character varying(50) NULL,
  data_dbl double precision NULL,
  data_lgl boolean NULL,
  data_chr character varying(50) NULL
);

SELECT * FROM instrument_logs;
