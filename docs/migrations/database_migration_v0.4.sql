-- TuoKit Database Migration Script
-- Adds verified column to existing knowledge_units table

-- Check if verified column exists, add if not
DO $$ 
BEGIN
    IF NOT EXISTS (
        SELECT column_name 
        FROM information_schema.columns 
        WHERE table_name='knowledge_units' AND column_name='verified'
    ) THEN
        ALTER TABLE knowledge_units ADD COLUMN verified BOOLEAN DEFAULT FALSE;
    END IF;
END $$;

-- Success message
SELECT 'Migration complete: verified column added to knowledge_units' as message;