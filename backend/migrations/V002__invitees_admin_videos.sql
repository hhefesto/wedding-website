-- V002: invitee management, admin sessions, and video submissions.

CREATE TABLE IF NOT EXISTS invitees
  ( id         BIGSERIAL   PRIMARY KEY
  , name       TEXT        NOT NULL
  , code       TEXT        UNIQUE
  , max_guests INT         NOT NULL DEFAULT 1 CHECK (max_guests BETWEEN 1 AND 20)
  , notes      TEXT
  , created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );

CREATE INDEX IF NOT EXISTS idx_invitees_lower_name ON invitees (LOWER(name));

CREATE TABLE IF NOT EXISTS admin_sessions
  ( token      TEXT        PRIMARY KEY
  , expires_at TIMESTAMPTZ NOT NULL
  );

ALTER TABLE rsvps ADD COLUMN IF NOT EXISTS invitee_id BIGINT REFERENCES invitees(id) ON DELETE SET NULL;
ALTER TABLE rsvps ADD COLUMN IF NOT EXISTS invitation_code_used TEXT;
ALTER TABLE rsvps DROP COLUMN IF EXISTS client_ip;
ALTER TABLE rsvps DROP COLUMN IF EXISTS user_agent;

CREATE TABLE IF NOT EXISTS videos
  ( id                UUID        PRIMARY KEY DEFAULT gen_random_uuid()
  , original_filename TEXT        NOT NULL
  , stored_filename   TEXT        NOT NULL UNIQUE
  , content_type      TEXT        NOT NULL
  , size_bytes        BIGINT      NOT NULL CHECK (size_bytes > 0)
  , submitter_name    TEXT
  , message           TEXT
  , created_at        TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );
