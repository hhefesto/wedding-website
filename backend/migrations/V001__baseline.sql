-- V001: initial schema. Mirrors the previous Db.hs:createSchema, kept idempotent
-- so it is a no-op on databases that were created before migrations existed.

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS rsvps
  ( id          UUID        PRIMARY KEY DEFAULT gen_random_uuid()
  , name        TEXT        NOT NULL
  , guest_count INT         NOT NULL CHECK (guest_count BETWEEN 1 AND 20)
  , dietary     TEXT
  , created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
  , client_ip   INET
  , user_agent  TEXT
  );
