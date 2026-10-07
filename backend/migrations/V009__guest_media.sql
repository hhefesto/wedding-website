-- V009: guest photo/video uploads shown in the live gallery.
--
-- Files live under WEDDING_MEDIA_DIR:
--   incoming/<id>.part        while a chunked upload is in progress
--   originals/<stored>        the untouched full-quality original
--   public/<id>-thumb.jpg     gallery tile / video poster (EXIF stripped)
--   public/<id>-display.jpg   lightbox photo (EXIF stripped)
--   public/<id>-preview.mp4   720p H.264 preview for videos

CREATE TABLE IF NOT EXISTS guest_media
  ( id                UUID        PRIMARY KEY
  , kind              TEXT        NOT NULL CHECK (kind IN ('photo', 'video'))
  , original_filename TEXT        NOT NULL
  , stored_filename   TEXT        NOT NULL UNIQUE
  , content_type      TEXT        NOT NULL
  , size_bytes        BIGINT      NOT NULL CHECK (size_bytes > 0)
  , status            TEXT        NOT NULL DEFAULT 'uploading'
                                  CHECK (status IN ('uploading', 'processing', 'ready', 'failed'))
  , hidden            BOOLEAN     NOT NULL DEFAULT FALSE
  , uploader_name     TEXT
  , ip_address        INET
  , width             INT
  , height            INT
  , duration_ms       BIGINT
  , created_at        TIMESTAMPTZ NOT NULL DEFAULT NOW()
  , ready_at          TIMESTAMPTZ
  );

CREATE INDEX IF NOT EXISTS idx_guest_media_public
  ON guest_media (ready_at DESC) WHERE status = 'ready' AND NOT hidden;
CREATE INDEX IF NOT EXISTS idx_guest_media_status ON guest_media (status, created_at);
