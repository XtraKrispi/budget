-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE Frequency AS ENUM ('OneTime', 'Weekly', 'BiWeekly', 'Monthly');
CREATE TYPE ItemAction AS ENUM ('Paid', 'Skipped');
CREATE TYPE AmountType AS ENUM ('Debit', 'Credit');
CREATE TABLE definitions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    description TEXT NOT NULL,
    amount DOUBLE PRECISION NOT NULL,
    amount_type AmountType NOT NULL,
    start_date DATE NOT NULL,
    end_date DATE DEFAULT NULL,
    frequency Frequency NOT NULL,
    is_deleted BOOLEAN DEFAULT false NOT NULL
);
CREATE TABLE archive (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    definition_id UUID NOT NULL,
    description TEXT NOT NULL,
    amount DOUBLE PRECISION NOT NULL,
    date DATE NOT NULL
);
CREATE INDEX archive_definition_id_index ON archive (definition_id);
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
ALTER TABLE archive ADD CONSTRAINT archive_ref_definition_id FOREIGN KEY (definition_id) REFERENCES definitions (id) ON DELETE NO ACTION;
