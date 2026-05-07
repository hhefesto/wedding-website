-- V006: allow upload sessions and videos to be linked to an RSVP, an invitee, or both.

ALTER TABLE videos ADD COLUMN IF NOT EXISTS rsvp_id UUID REFERENCES rsvps(id) ON DELETE SET NULL;
CREATE INDEX IF NOT EXISTS idx_videos_rsvp_id ON videos (rsvp_id);

ALTER TABLE rsvp_sessions ADD COLUMN IF NOT EXISTS rsvp_id UUID REFERENCES rsvps(id) ON DELETE CASCADE;
ALTER TABLE rsvp_sessions ALTER COLUMN invitee_id DROP NOT NULL;
CREATE INDEX IF NOT EXISTS idx_rsvp_sessions_rsvp_id ON rsvp_sessions (rsvp_id);

DO $$
BEGIN
  ALTER TABLE rsvp_sessions DROP CONSTRAINT IF EXISTS rsvp_sessions_has_link_check;
  ALTER TABLE rsvp_sessions ADD CONSTRAINT rsvp_sessions_has_link_check
    CHECK (invitee_id IS NOT NULL OR rsvp_id IS NOT NULL);
END $$;
