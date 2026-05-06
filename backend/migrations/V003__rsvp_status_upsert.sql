-- V003: make RSVP a real response: attending or declined, keyed by invitee.

ALTER TABLE rsvps ADD COLUMN IF NOT EXISTS status TEXT NOT NULL DEFAULT 'attending';
ALTER TABLE rsvps ADD COLUMN IF NOT EXISTS guest_names TEXT[] NOT NULL DEFAULT '{}';
ALTER TABLE rsvps ADD COLUMN IF NOT EXISTS updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW();

DO $$
BEGIN
  ALTER TABLE rsvps DROP CONSTRAINT IF EXISTS rsvps_guest_count_check;
  ALTER TABLE rsvps ADD CONSTRAINT rsvps_guest_count_check CHECK (guest_count BETWEEN 0 AND 20);
  ALTER TABLE rsvps DROP CONSTRAINT IF EXISTS rsvps_status_check;
  ALTER TABLE rsvps ADD CONSTRAINT rsvps_status_check CHECK (status IN ('attending', 'declined'));
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS idx_rsvps_invitee_once
  ON rsvps (invitee_id)
  WHERE invitee_id IS NOT NULL;
