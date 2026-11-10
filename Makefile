backup-db:
	supabase db dump --remote="postgresql://postgres.[PROJECT]:[PASSWORD]@aws-1-us-west-2.pooler.supabase.com:5432/postgres" -f roles.sql --role-only
	supabase db dump --remote="postgresql://postgres.[PROJECT]:[PASSWORD]@aws-1-us-west-2.pooler.supabase.com:5432/postgres" -f schema.sql
	supabase db dump --remote="postgresql://postgres.[PROJECT]:[PASSWORD]@aws-1-us-west-2.pooler.supabase.com:5432/postgres" -f data.sql --use-copy --data-only -x "storage.buckets_vectors" -x "storage.vector_indexes"

restore-db:
	psql \
		--single-transaction \
		--variable ON_ERROR_STOP=1 \
		--file roles.sql \
		--file schema.sql \
		--command 'SET session_replication_role = replica' \
		--file data.sql \
		--dbname "postgresql://postgres.[PROJECT]:[PASSWORD]@aws-0-us-west-2.pooler.supabase.com:5432/postgres"