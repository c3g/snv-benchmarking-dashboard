# db_interface.py
"""
Core SQLAlchemy interface for Benchmarking Dashboard - translator between Rshiny and the DB
Provides functions for R Shiny dashboard (with Reticulate)

Main Functions:
- get_experiments() - Core experiment data with filtering
- get_results() - Benchmark results data  
- get_performance_summary() - Aggregated performance metrics
- get_filter_options() - Dropdown values for UI
- test_connection() - Database connectivity check
"""

import pandas as pd
from sqlalchemy.orm import joinedload
from database import get_db_session
from models import *

# ========================================================================
# 1. BASIC EXPERIMENTS OVERVIEW (Lightweight)
# ========================================================================

def get_experiments_overview(filters=None):
    """
    Get basic experiment information - optimized for dashboard overview.
    Returns essential info without heavy joins.
    """
    try:
        with get_db_session() as session:
            # Basic query with minimal joins
            query = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology),
                joinedload(Experiment.variant_caller),
                joinedload(Experiment.truth_set)
            )
            
            # Apply basic filters
            if filters:
                query = apply_basic_filters(query, filters)
            
            experiments = query.all()
            
            # Return minimal essential data
            result_data = []
            for exp in experiments:
                result_data.append({
                    'id': exp.id,
                    'name': exp.name,
                    'technology': exp.sequencing_technology.technology.value if exp.sequencing_technology else None,
                    'platform': exp.sequencing_technology.platform_name if exp.sequencing_technology else None,
                    'caller': exp.variant_caller.name.value if exp.variant_caller else None,
                    'caller_version': exp.variant_caller.version if exp.variant_caller else None,
                    'truth_set': exp.truth_set.name.value if exp.truth_set else None,
                    'sample': exp.truth_set.sample.value if exp.truth_set else None,
                    'created_at': exp.created_at.strftime('%Y-%m-%d') if exp.created_at else None
                })
            
            return {
                "success": True,
                "data": result_data,
                "count": len(result_data)
            }
            
    except Exception as e:
        print(f"Error in get_experiments_overview: {e}")
        return {"success": False, "error": str(e), "data": []}

# ========================================================================
# 2. DETAILED EXPERIMENT INFO (For selected experiments)
# ========================================================================

def get_experiment_details(experiment_ids):
    """
    Get complete details for specific experiments.
    Use this when user selects specific experiments for detailed view.
    """
    try:
        with get_db_session() as session:
            # Comprehensive query - but only for selected experiments
            query = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology),
                joinedload(Experiment.variant_caller),
                joinedload(Experiment.aligner),
                joinedload(Experiment.truth_set),
                joinedload(Experiment.benchmark_tool),
                joinedload(Experiment.variant),
                joinedload(Experiment.chemistry),
                joinedload(Experiment.quality_control)
            ).filter(Experiment.id.in_(experiment_ids))
            
            experiments = query.all()
            
            # Detailed data extraction
            result_data = []
            for exp in experiments:
                result_data.append({
                    'id': exp.id,
                    'name': exp.name,
                    'description': exp.description,
                    'created_at': exp.created_at.isoformat() if exp.created_at else None,
                    
                    # Technology info
                    'technology': exp.sequencing_technology.technology.value if exp.sequencing_technology else None,
                    'platform_name': exp.sequencing_technology.platform_name if exp.sequencing_technology else None,
                    'platform_type': exp.sequencing_technology.platform_type.value if exp.sequencing_technology else None,
                    'target': exp.sequencing_technology.target.value if exp.sequencing_technology else None,
                    
                    # Caller info
                    'caller_name': exp.variant_caller.name.value if exp.variant_caller else None,
                    'caller_type': exp.variant_caller.type.value if exp.variant_caller else None,
                    'caller_version': exp.variant_caller.version if exp.variant_caller else None,
                    'caller_model': exp.variant_caller.model if exp.variant_caller else None,
                    
                    # Aligner info
                    'aligner_name': exp.aligner.name if exp.aligner else None,
                    'aligner_version': exp.aligner.version if exp.aligner else None,
                    
                    # Truth set info
                    'truth_set_name': exp.truth_set.name.value if exp.truth_set else None,
                    'truth_set_sample': exp.truth_set.sample.value if exp.truth_set else None,
                    'truth_set_version': exp.truth_set.version if exp.truth_set else None,
                    'truth_set_reference': exp.truth_set.reference.value if exp.truth_set else None,
                    
                    # Benchmark tool
                    'benchmark_tool': exp.benchmark_tool.name.value if exp.benchmark_tool else None,
                    'benchmark_tool_version': exp.benchmark_tool.version if exp.benchmark_tool else None,
                    
                    # Variant info
                    'variant_type': exp.variant.type.value if exp.variant else None,
                    'variant_origin': exp.variant.origin.value if exp.variant else None,
                    'variant_size': exp.variant.size.value if exp.variant else None,
                    'is_phased': exp.variant.is_phased if exp.variant else None,
                    
                    # Quality metrics - Handle None values properly
                    'mean_coverage': float(exp.quality_control.mean_coverage) if (exp.quality_control and exp.quality_control.mean_coverage is not None) else None,
                    'read_length': float(exp.quality_control.read_length) if (exp.quality_control and exp.quality_control.read_length is not None) else None,
                    'mean_read_length': float(exp.quality_control.mean_read_length) if (exp.quality_control and exp.quality_control.mean_read_length is not None) else None,
                    'mean_insert_size': float(exp.quality_control.mean_insert_size) if (exp.quality_control and exp.quality_control.mean_insert_size is not None) else None,
                    
                    # Chemistry
                    'chemistry_name': exp.chemistry.name if exp.chemistry else None,
                    'chemistry_version': exp.chemistry.version if exp.chemistry else None,
                })
            
            return {
                "success": True,
                "data": result_data,
                "count": len(result_data)
            }
            
    except Exception as e:
        print(f"Error in get_experiment_details: {e}")
        return {"success": False, "error": str(e), "data": []}

# ========================================================================
# 3. BENCHMARK RESULTS (Performance data only)
# ========================================================================

def get_benchmark_results(experiment_ids=None, variant_types=None):
    """
    Get performance results - separate from metadata for efficiency
    """
    try:
        with get_db_session() as session:
            query = session.query(BenchmarkResult).options(
                joinedload(BenchmarkResult.experiment)
            )
            
            if experiment_ids:
                query = query.filter(BenchmarkResult.experiment_id.in_(experiment_ids))
            
            if variant_types:
                query = query.filter(BenchmarkResult.variant_type.in_(variant_types))
            
            # Focus on overall results (not stratified)
            query = query.filter(BenchmarkResult.subset == 'ALL_REGIONS')
            
            results = query.all()
            
            data = []
            for result in results:
                data.append({
                    'experiment_id': result.experiment_id,
                    'experiment_name': result.experiment.name if result.experiment else None,
                    'variant_type': result.variant_type,
                    'recall': result.metric_recall,
                    'precision': result.metric_precision,
                    'f1_score': result.metric_f1_score,
                    'truth_total': result.truth_total,
                    'truth_tp': result.truth_tp,
                    'truth_fn': result.truth_fn,
                    'query_fp': result.query_fp
                })
            
            return {
                "success": True,
                "data": data,
                "count": len(data)
            }
            
    except Exception as e:
        print(f"Error in get_benchmark_results: {e}")
        return {"success": False, "error": str(e), "data": []}

# ========================================================================
# 4. FILTER OPTIONS (For dropdowns)
# ========================================================================

def get_filter_options():
    """
    Get unique values for dashboard filters - lightweight query
    """
    try:
        with get_db_session() as session:
            # Get unique technologies
            tech_query = session.query(SequencingTechnology.technology).distinct()
            technologies = [tech[0].value for tech in tech_query.all() if tech[0]]
            
            # Get unique callers
            caller_query = session.query(VariantCaller.name, VariantCaller.version).distinct()
            callers = [f"{caller[0].value} v{caller[1]}" for caller in caller_query.all() if caller[0]]
            
            # Get unique truth sets
            truth_query = session.query(TruthSet.name).distinct()
            truth_sets = [ts[0].value for ts in truth_query.all() if ts[0]]
            
            # Get unique samples
            sample_query = session.query(TruthSet.sample).distinct()
            samples = [s[0].value for s in sample_query.all() if s[0]]
            
            return {
                "success": True,
                "data": {
                    "technologies": technologies,
                    "callers": callers,
                    "truth_sets": truth_sets,
                    "samples": samples
                }
            }
            
    except Exception as e:
        print(f"Error in get_filter_options: {e}")
        return {"success": False, "error": str(e), "data": {}}

# ========================================================================
# 5. HELPER FUNCTIONS
# ========================================================================

def apply_basic_filters(query, filters):
    """
    Apply basic filters to query - with proper error handling
    """
    try:
        # Technology filter - Handle enum conversion errors
        if filters.get('technology'):
            valid_techs = []
            for tech in filters['technology']:
                try:
                    valid_techs.append(SeqTechName(tech))
                except ValueError:
                    print(f"Warning: Invalid technology value: {tech}")
            
            if valid_techs:
                # Check if we already joined SequencingTechnology
                joined_tables = [str(join) for join in query.column_descriptions]
                if 'sequencing_technologies' not in str(joined_tables):
                    query = query.join(SequencingTechnology)
                query = query.filter(SequencingTechnology.technology.in_(valid_techs))
        
        # Caller filter - Similar error handling
        if filters.get('caller'):
            valid_callers = []
            for caller in filters['caller']:
                try:
                    valid_callers.append(CallerName(caller))
                except ValueError:
                    print(f"Warning: Invalid caller value: {caller}")
            
            if valid_callers:
                joined_tables = [str(join) for join in query.column_descriptions]
                if 'variant_callers' not in str(joined_tables):
                    query = query.join(VariantCaller)
                query = query.filter(VariantCaller.name.in_(valid_callers))
        
        # Coverage filters
        if filters.get('min_coverage') or filters.get('max_coverage'):
            joined_tables = [str(join) for join in query.column_descriptions]
            if 'quality_control_metrics' not in str(joined_tables):
                query = query.join(QualityControl)
            
            if filters.get('min_coverage'):
                query = query.filter(QualityControl.mean_coverage >= filters['min_coverage'])
            if filters.get('max_coverage'):
                query = query.filter(QualityControl.mean_coverage <= filters['max_coverage'])
                
        return query
        
    except Exception as e:
        print(f"Error applying filters: {e}")
        return query

# ========================================================================
# 6. CONVENIENCE FUNCTIONS FOR R (Return DataFrames)
# ========================================================================

def get_experiments_dataframe(filters=None):
    """Return experiments as pandas DataFrame for R"""
    result = get_experiments_overview(filters)
    if result["success"]:
        return pd.DataFrame(result["data"])
    else:
        return pd.DataFrame()

def get_results_dataframe(experiment_ids=None, variant_types=None):
    """Return results as pandas DataFrame for R"""
    result = get_benchmark_results(experiment_ids, variant_types)
    if result["success"]:
        return pd.DataFrame(result["data"])
    else:
        return pd.DataFrame()

def get_details_dataframe(experiment_ids):
    """Return detailed experiment info as pandas DataFrame for R"""
    result = get_experiment_details(experiment_ids)
    if result["success"]:
        return pd.DataFrame(result["data"])
    else:
        return pd.DataFrame()

# ========================================================================
# 7. SIMPLE WRAPPER FUNCTIONS FOR R TO CALL DIRECTLY
# ========================================================================

def get_experiments(filters=None):
    """Simple function for R to get experiments"""
    return get_experiments_dataframe(filters)

def get_results(experiment_ids=None, variant_types=None):
    """Simple function for R to get results"""
    return get_results_dataframe(experiment_ids, variant_types)

def get_details(experiment_ids):
    """Simple function for R to get detailed experiment info"""
    return get_details_dataframe(experiment_ids)

def get_options():
    """Simple function for R to get filter options"""
    return get_filter_options()

# ========================================================================
# 8. PERFORMANCE COMPARISON FUNCTION (For plotting)
# ========================================================================

def get_performance_comparison_data(metric='f1_score'):
    """
    Get performance comparison data optimized for plotting in R
    """
    try:
        with get_db_session() as session:
            # Join query to get all relevant info in one go
            query = session.query(
                BenchmarkResult.variant_type,
                BenchmarkResult.metric_recall,
                BenchmarkResult.metric_precision,
                BenchmarkResult.metric_f1_score,
                Experiment.name.label('experiment_name'),
                SequencingTechnology.technology.label('technology'),
                SequencingTechnology.platform_name.label('platform'),
                VariantCaller.name.label('caller'),
                VariantCaller.version.label('caller_version')
            ).join(
                Experiment, BenchmarkResult.experiment_id == Experiment.id
            ).join(
                SequencingTechnology, Experiment.sequencing_technology_id == SequencingTechnology.id
            ).join(
                VariantCaller, Experiment.variant_caller_id == VariantCaller.id
            ).filter(
                BenchmarkResult.subset == 'ALL_REGIONS'  # Focus on overall performance
            )
            
            results = query.all()
            
            # Convert to DataFrame
            data = []
            for row in results:
                data.append({
                    'variant_type': row.variant_type,
                    'recall': row.metric_recall,
                    'precision': row.metric_precision,
                    'f1_score': row.metric_f1_score,
                    'experiment_name': row.experiment_name,
                    'technology': row.technology.value if row.technology else None,
                    'platform': row.platform,
                    'caller': row.caller.value if row.caller else None,
                    'caller_version': row.caller_version,
                    'tech_caller': f"{row.technology.value if row.technology else 'Unknown'}_{row.caller.value if row.caller else 'Unknown'}"
                })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        print(f"Error getting comparison data: {e}")
        return pd.DataFrame()

def get_plot_data(metric='f1_score'):
    """Simple wrapper for R plotting"""
    return get_performance_comparison_data(metric)
'''
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

            # Apply filters if provided -------------- ADD other filters --------------------------------------------------
            if filters:

                # Sequencing Technology filters:
                if filters.get('technology'):
                    # Converts strings given as arqument into enums used for querying 
                    tech_enums = [SeqTechName(tech) for tech in filters['technology'] if tech]
                    query = query.join(SequencingTechnology).filter(  # joins the selected te
                        SequencingTechnology.technology.in_(tech_enums)
                    )

                # Caller filter  
                if filters.get('caller'):
                    caller_enums = [CallerName(caller) for caller in filters['caller'] if caller]
                    query = query.join(VariantCaller).filter(
                        VariantCaller.name.in_(caller_enums)
                    )

            # store all queries as experiment
            experiments = query.all() 

            results = []

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

'''