-- V007: capture submission IPs and resolve RSVP/video submissions to invitations.

ALTER TABLE rsvps ADD COLUMN IF NOT EXISTS ip_address INET;
ALTER TABLE rsvps ADD COLUMN IF NOT EXISTS invitee_resolution_status TEXT NOT NULL DEFAULT 'unlinked';
ALTER TABLE rsvps ADD COLUMN IF NOT EXISTS suggested_invitee_id BIGINT REFERENCES invitees(id) ON DELETE SET NULL;
ALTER TABLE rsvps ADD COLUMN IF NOT EXISTS suggested_existing_rsvp_id UUID REFERENCES rsvps(id) ON DELETE SET NULL;

ALTER TABLE videos ADD COLUMN IF NOT EXISTS ip_address INET;
ALTER TABLE videos ADD COLUMN IF NOT EXISTS invitee_resolution_status TEXT NOT NULL DEFAULT 'unlinked';

CREATE TABLE IF NOT EXISTS invitee_ip_addresses
  ( invitee_id    BIGINT      NOT NULL REFERENCES invitees(id) ON DELETE CASCADE
  , ip_address    INET        NOT NULL
  , first_seen_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  , last_seen_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
  , source        TEXT        NOT NULL DEFAULT 'submission'
  , PRIMARY KEY (invitee_id, ip_address)
  );

DO $$
BEGIN
  ALTER TABLE rsvps DROP CONSTRAINT IF EXISTS rsvps_invitee_resolution_status_check;
  ALTER TABLE rsvps ADD CONSTRAINT rsvps_invitee_resolution_status_check
    CHECK (invitee_resolution_status IN ('unlinked', 'resolved', 'review'));

  ALTER TABLE videos DROP CONSTRAINT IF EXISTS videos_invitee_resolution_status_check;
  ALTER TABLE videos ADD CONSTRAINT videos_invitee_resolution_status_check
    CHECK (invitee_resolution_status IN ('unlinked', 'resolved', 'review'));
END $$;

CREATE INDEX IF NOT EXISTS idx_rsvps_ip_address ON rsvps (ip_address);
CREATE INDEX IF NOT EXISTS idx_rsvps_suggested_invitee_id ON rsvps (suggested_invitee_id);
CREATE INDEX IF NOT EXISTS idx_rsvps_suggested_existing_rsvp_id ON rsvps (suggested_existing_rsvp_id);
CREATE INDEX IF NOT EXISTS idx_videos_ip_address ON videos (ip_address);
CREATE INDEX IF NOT EXISTS idx_invitee_ip_addresses_ip ON invitee_ip_addresses (ip_address, first_seen_at, invitee_id);

UPDATE rsvps SET invitee_resolution_status = 'resolved' WHERE invitee_id IS NOT NULL;
UPDATE videos SET invitee_resolution_status = 'resolved' WHERE invitee_id IS NOT NULL;
