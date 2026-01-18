"""partition_and_indexing

Revision ID: 40fb186cd43a
Revises: e67f64ad2817
Create Date: 2026-01-17 13:51:13.526869

"""
from alembic import op
import sqlalchemy as sa
from datetime import date


# revision identifiers, used by Alembic.
revision = '40fb186cd43a'
down_revision = 'e67f64ad2817'
branch_labels = None
depends_on = None


def _next_month(d: date) -> date:
    return date(d.year + (d.month == 12), 1 if d.month == 12 else d.month + 1, 1)

def upgrade() -> None:
    # 1. Clean up any existing indexes to avoid "already exists" errors
    op.execute("DROP INDEX IF EXISTS idx_migration_get_dates CASCADE")
    op.execute("DROP INDEX IF EXISTS idx_population_get_dates CASCADE")
    op.execute("DROP INDEX IF EXISTS idx_get_migration_prob CASCADE")
    op.execute("DROP INDEX IF EXISTS idx_get_population CASCADE")

    # 2. Create partitions for a range of months
    start = date(2022, 1, 1)
    end = date(2027, 1, 1)

    d = start
    while d < end:
        nxt = _next_month(d)
        op.execute(
            sa.sql.text(f"""
            CREATE TABLE IF NOT EXISTS population_{d:%Y%m}
            PARTITION OF population
            FOR VALUES FROM (:d1) TO (:d2);
        """).bindparams(d1=d, d2=nxt)
        )

        op.execute(
            sa.sql.text(f"""
            CREATE TABLE IF NOT EXISTS migration_{d:%Y%m}
            PARTITION OF migration
            FOR VALUES FROM (:d1) TO (:d2);
        """).bindparams(d1=d, d2=nxt)
        )

        d = nxt

    # add default partitions
    op.execute(
        "CREATE TABLE IF NOT EXISTS population_default PARTITION OF population DEFAULT;"
    )
    op.execute(
        "CREATE TABLE IF NOT EXISTS migration_default PARTITION OF migration DEFAULT;"
    )

    # 2. optimise get_dates()
    op.execute("CREATE INDEX idx_migration_get_dates ON migration (country, day)")
    op.execute("CREATE INDEX idx_population_get_dates ON population (country, day)")

    # 3. optimise get_migration_probabilities()
    # Note: No 'ONLY' here ensures it covers all monthly partitions
    op.execute("""
        CREATE INDEX idx_get_migration_prob 
        ON migration (country, admin_level, origin, destination, sex, age_min, age_max) 
        INCLUDE (proportion, count)
    """)

    # 4. optimise get_population()
    op.execute("""
        CREATE INDEX idx_get_population 
        ON population (country, admin_level, pcode, age_min, age_max, sex)
    """)

    # 5. Finalize and update stats
    op.execute("COMMIT")
    op.execute("ANALYZE migration")
    op.execute("ANALYZE population")


def downgrade() -> None:
    op.execute("DROP INDEX IF EXISTS idx_migration_get_dates")
    op.execute("DROP INDEX IF EXISTS idx_population_get_dates")
    op.execute("DROP INDEX IF EXISTS idx_get_migration_prob")
    op.execute("DROP INDEX IF EXISTS idx_get_population")