-- V008: give invitation/IP associations stable IDs for admin management.

ALTER TABLE invitee_ip_addresses ADD COLUMN IF NOT EXISTS id BIGSERIAL;

DO $$
BEGIN
  ALTER TABLE invitee_ip_addresses DROP CONSTRAINT IF EXISTS invitee_ip_addresses_id_unique;
  ALTER TABLE invitee_ip_addresses ADD CONSTRAINT invitee_ip_addresses_id_unique UNIQUE (id);
END $$;

CREATE INDEX IF NOT EXISTS idx_invitee_ip_addresses_invitee ON invitee_ip_addresses (invitee_id, last_seen_at DESC);
