# db_interface.py
"""
Core SQLAlchemy interface for Benchmarking Dashboard - translator between Rshiny and the DB
Provides essential functions for R Shiny dashboard (with Reticulate)

Main Functions:
- get_experiments() - Core experiment data with filtering
- get_results() - Benchmark results data  
- get_performance_summary() - Aggregated performance metrics
- get_filter_options() - Dropdown values for UI
- test_connection() - Database connectivity check
"""

from database import get_db_session, test_connection
from models import *
from sqlalchemy.orm import *
from sqlalchemy import *
import traceback


# ============================================================================
# MAIN DATA FUNCTIONS
# ============================================================================

def get_experiments(filters=None):
    """
    Get experiments with optional filtering
    
    Args:
        filters (dict): Optional filters
            - technology: list of technology names ["Illumina", "PacBio"]
            - caller: list of caller names ["DeepVariant", "GATK"] 
            - min_coverage: minimum coverage threshold
            - max_coverage: maximum coverage threshold
            - truth_set: list of truth set names ["GIAB"]
            
    Returns:
        dict: {"success": bool, "data": list, "count": int}
    """

    try:
        with get_db_session() as session:
            # start the query on the main experiment table and load the complete database
            query = session.query(Experiment).options(
                # joins all related metadata for retrieval
                joinedload(Experiment.sequencing_technology),
                joinedload(Experiment.variant_caller),
                joinedload(Experiment.aligner),
                joinedload(Experiment.truth_set),
                joinedload(Experiment.benchmark_tool),
                joinedload(Experiment.variant),
                joinedload(Experiment.chemistry),
                joinedload(Experiment.quality_control)
            )

            # Apply filters if provided
            if filters:
                # Sequencing Technology filters:
                if filters.get('technology'):
                    # Converts strings given as arqument into enums used for query
                    tech_enums = [SeqTechName(tech) for tech in filters['technology'] if tech]
                    query = query.join(SequencingTechnology).filter(
                        SequencingTechnology.technology.in_(tech_enums)
                    )
            results = []

            # test --
            experiments = query.all()
            for exp in experiments:
                results.append({
                    'id' : exp.id,
                    'tech' : exp.sequencing_technology.technology.value if exp.sequencing_technology else None,
                     'caller_name': exp.variant_caller.name.value if exp.variant_caller else None,
                })


            return(results)

    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "traceback": traceback.format_exc()
        }

#+++++++++++++++++++++++++++++
#TEST
#+++++++++++++++++++++++++++++

if __name__ == "__main__":
    # Test connection
    test_connection()
    
    print(get_experiments())