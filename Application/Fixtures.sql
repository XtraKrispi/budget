

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.definitions DISABLE TRIGGER ALL;

INSERT INTO public.definitions (id, description, amount, amount_type, start_date, end_date, frequency, is_deleted) VALUES ('33f40247-d8d9-4793-a69d-aab167577076', 'Mortgage', 1481.69000000000005, 'Debit', '2021-06-28', NULL, 'OneTime', false);
INSERT INTO public.definitions (id, description, amount, amount_type, start_date, end_date, frequency, is_deleted) VALUES ('0c1c1143-303d-45de-a685-1a2dc9e6212c', 'Payday', 5300, 'Credit', '2021-07-02', NULL, 'BiWeekly', false);


ALTER TABLE public.definitions ENABLE TRIGGER ALL;


ALTER TABLE public.archive DISABLE TRIGGER ALL;



ALTER TABLE public.archive ENABLE TRIGGER ALL;


