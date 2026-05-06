-- V004: RSVP sessions gate guest video uploads and connect videos to invitees.

CREATE TABLE IF NOT EXISTS rsvp_sessions
  ( token      TEXT        PRIMARY KEY
  , invitee_id BIGINT      NOT NULL REFERENCES invitees(id) ON DELETE CASCADE
  , expires_at TIMESTAMPTZ NOT NULL
  );

CREATE INDEX IF NOT EXISTS idx_rsvp_sessions_invitee_id ON rsvp_sessions (invitee_id);
CREATE INDEX IF NOT EXISTS idx_rsvp_sessions_expires_at ON rsvp_sessions (expires_at);

ALTER TABLE videos ADD COLUMN IF NOT EXISTS invitee_id BIGINT REFERENCES invitees(id) ON DELETE SET NULL;
CREATE INDEX IF NOT EXISTS idx_videos_invitee_id ON videos (invitee_id);
