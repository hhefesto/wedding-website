-- V005: RSVP can be unlinked and invitations are capped to two adults.

UPDATE invitees SET max_guests = LEAST(max_guests, 2);
UPDATE rsvps SET guest_count = LEAST(guest_count, 2) WHERE status = 'attending';

DO $$
BEGIN
  ALTER TABLE invitees DROP CONSTRAINT IF EXISTS invitees_max_guests_check;
  ALTER TABLE invitees ADD CONSTRAINT invitees_max_guests_check CHECK (max_guests BETWEEN 1 AND 2);
END $$;
