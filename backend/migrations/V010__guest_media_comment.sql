-- V010: an optional guest comment sent with a photo/video upload, shown with
-- it in the gallery. One comment covers every file picked together.

ALTER TABLE guest_media ADD COLUMN IF NOT EXISTS comment TEXT;

DO $$
BEGIN
  ALTER TABLE guest_media DROP CONSTRAINT IF EXISTS guest_media_comment_length_check;
  ALTER TABLE guest_media ADD CONSTRAINT guest_media_comment_length_check
    CHECK (comment IS NULL OR char_length(comment) <= 500);
END $$;
