"""add_loose_index_scan_support

Revision ID: 40fb186cd43a
Revises: e67f64ad2817
Create Date: 2026-01-17 13:51:13.526869

"""
from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = '40fb186cd43a'
down_revision = 'e67f64ad2817'
branch_labels = None
depends_on = None


def upgrade() -> None:
    # Adding the composite indexes to support the Recursive CTE (Loose Index Scan)
    # Note: We do not use 'concurrently' because these are partitioned tables
    op.drop_index("ix_population_country_pcode", table_name="population", if_exists=True)
    op.drop_index(
        "ix_population_admin_country_age_sex", table_name="population", if_exists=True
    )
    op.drop_index(
        "ix_migration_country_origin_dest", table_name="migration", if_exists=True
    )
    op.drop_index(
        "ix_migration_admin_country_OD_age_sex", table_name="migration", if_exists=True
    )
    
    op.create_index("idx_migration_country_day", "migration", ["country", "day"])
    op.create_index("idx_population_country_day", "population", ["country", "day"])

    op.create_index(
        "idx_pop_lookup",
        "population",
        ["country", "admin_level", "pcode", "age_min", "age_max", "sex"],
    )
    op.create_index(
        "idx_migration_lookup",
        "migration",
        ["country", "admin_level", "origin", "sex", "age_min", "age_max", "proportion", "count"],
    )


def downgrade() -> None:
    op.drop_index("idx_migration_country_day", table_name="migration")
    op.drop_index("idx_population_country_day", table_name="population")

    op.drop_index("idx_migration_lookup", table_name="migration")
    op.drop_index("idx_pop_lookup", table_name="population")

    op.create_index("ix_population_country_pcode", "population", ["country", "pcode"])
    op.create_index("ix_population_admin_country_age_sex", "population", ["admin_level", "country", "age_min", "age_max","sex"])

    op.create_index("ix_migration_country_origin_dest", "migration", ["country", "origin", "destination"])
    op.create_index("ix_migration_admin_country_OD_age_sex", "migration", ["admin_level", "country", "origin", "destination", "age_min","age_max", "sex"])


