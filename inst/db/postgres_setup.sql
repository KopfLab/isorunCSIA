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
  instrument_id character varying(20) PRIMARY KEY,
  instrument_description text NULL,
  location character varying(100) NULL
);

INSERT INTO instruments(instrument_id, instrument_description)
	VALUES ('TEST', 'Test Instrument');

SELECT * FROM instruments;

-- Table: instrument_modes

DROP TABLE IF EXISTS instrument_modes;

CREATE TABLE instrument_modes
(
  instrument_id character varying(20) NOT NULL references instruments(instrument_id) on update cascade,
  mode_id character varying(20) NOT NULL,
  mode_name text NOT NULL,
  PRIMARY KEY (instrument_id, mode_id)
);

INSERT INTO instrument_modes(instrument_id, mode_id, mode_name)
	VALUES ('TEST', 'mode1', 'test mode 1'), ('TEST', 'mode2', 'test mode 2');

SELECT * FROM instrument_modes;

-- Table: sessions

DROP TABLE IF EXISTS sessions;

CREATE TABLE sessions
(
  session_id SERIAL,
  session_description text NULL,
  instrument_id character varying(20) NOT NULL,
  mode_id character varying(20) NOT NULL,
  FOREIGN KEY (instrument_id, mode_id) REFERENCES instrument_modes (instrument_id, mode_id) on update cascade,
  datetime timestamp with time zone,
  login character varying(20) NULL,
  PRIMARY KEY (session_id, instrument_id, mode_id)
);

-- Table: instrument_log_categories

DROP TABLE IF EXISTS instrument_log_categories;

DROP SEQUENCE IF EXISTS instrument_log_categories_sorting;
CREATE SEQUENCE instrument_log_categories_sorting START 1;

CREATE TABLE instrument_log_categories
(
  category_id character varying(20) NOT NULL,
  instrument_id character varying(20) NOT NULL REFERENCES instruments(instrument_id) on update cascade,
  category_sorting integer NOT NULL DEFAULT NEXTVAL('instrument_log_categories_sorting'),
  PRIMARY KEY (category_id, instrument_id)
);

-- Table: parameter_types

DROP TABLE IF EXISTS instrument_log_field_types;

CREATE TABLE instrument_log_field_types
(
  field_type varchar(20) PRIMARY KEY
);

INSERT INTO instrument_log_field_types VALUES ('checkbox'), ('numeric');

-- Table: instrument_log_fields

DROP TABLE IF EXISTS instrument_log_fields;

DROP SEQUENCE IF EXISTS instrument_log_fields_sorting;
CREATE SEQUENCE instrument_log_fields_sorting START 1;

CREATE TABLE instrument_log_fields
(
  field_id SERIAL,
  category_id character varying(20) NOT NULL,
  instrument_id character varying(20) NOT NULL,
  FOREIGN KEY (category_id, instrument_id) REFERENCES instrument_log_categories (category_id, instrument_id) on update cascade,
  field_sorting integer NOT NULL DEFAULT NEXTVAL('instrument_log_fields_sorting'),
  field_caption text NOT NULL,
  field_type varchar(20) NOT NULL REFERENCES instrument_log_field_types(field_type),
  field_unit varchar(20) NULL,
  field_information text NULL,
  PRIMARY KEY (instrument_id, field_id),
  UNIQUE(category_id, instrument_id, field_caption)
);

-- Table: instrument_mode_fields

DROP TABLE IF EXISTS instrument_mode_log_fields;

CREATE TABLE instrument_mode_log_fields
(
  field_id integer NOT NULL,
  instrument_id character varying(20) NOT NULL,
  mode_id character varying(20) NOT NULL,
  FOREIGN KEY (instrument_id, field_id) REFERENCES instrument_log_fields (instrument_id, field_id) on update cascade,
  FOREIGN KEY (instrument_id, mode_id) REFERENCES instrument_modes (instrument_id, mode_id) on update cascade
);

-- Table: instrument_logs

DROP TABLE IF EXISTS instrument_logs;

CREATE TABLE instrument_logs
(
  log_id SERIAL PRIMARY KEY,
  instrument_id character varying(20) NOT NULL,
  field_id integer NOT NULL,
  FOREIGN KEY (instrument_id, field_id) REFERENCES instrument_log_fields (instrument_id, field_id) on update cascade,
  mode_id character varying(20) NOT NULL,
  FOREIGN KEY (instrument_id, mode_id) REFERENCES instrument_modes (instrument_id, mode_id) on update cascade,
  session_id integer NOT NULL,
  FOREIGN KEY (instrument_id, session_id, mode_id) REFERENCES sessions (instrument_id, session_id, mode_id) on update cascade,
  data_dbl double precision NULL,
  data_lgl boolean NULL,
  notes text NULL,
  UNIQUE (instrument_id, field_id, mode_id, session_id)
);

SELECT * FROM instrument_logs;


