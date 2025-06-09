from database import get_db_session
from models import *

def test_connection():
    """Test if we can connect to the database"""
    try:
        with get_db_session() as session:
            count = session.query(Experiment).count()
            return {
                "success": True,
                "message": f"Connected! Found {count} experiments"
            }
    except Exception as e:
        return {
            "success": False,
            "error": str(e)
        }

def get_f1_scores():
    """Get F1 scores for visualization"""
    try:
        with get_db_session() as session:
            results = session.query(
                Experiment.name,
                BenchmarkResult.variant_type,
                BenchmarkResult.metric_f1_score
            ).join(BenchmarkResult).all()
            
            # Convert to simple list of dictionaries
            data = []
            for exp_name, variant_type, f1_score in results:
                if f1_score is not None:
                    data.append({
                        'experiment': exp_name,
                        'variant_type': variant_type,
                        'f1_score': f1_score
                    })
            
            return data
            
    except Exception as e:
        print(f"Error getting F1 scores: {e}")
        return []