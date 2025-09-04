# ============================================================================
# models.py
# ============================================================================
"""
Database models and enums for SNV Benchmarking Dashboard.

Main components:
- Enum definitions for all categorical data types
- SQLAlchemy table models for normalized database structure
- Relationship definitions between tables
- RegionType enum with hap.py mapping methods
"""

from sqlalchemy import Column, Integer, String, Float, DateTime, Boolean, ForeignKey, Enum, func
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
import enum

Base = declarative_base()

# ============================================================================
# ENUM DEFINITIONS - All uppercase
# ============================================================================

class SeqTechName(enum.Enum):
    """Sequencing technologies"""
    ILLUMINA = "ILLUMINA"
    MGI = "MGI"
    ONT = "ONT"
    PACBIO = "PACBIO"

class SeqTechTarget(enum.Enum):
    """Sequencing targets"""
    WGS = "WGS" # whole genome sequencing
    WES = "WES" # exon sequencing    

class SeqTechPlatformType(enum.Enum):
    """Sequencing Platform types"""
    SRS = "SRS"  # Short Read Sequencing
    LRS = "LRS" # Long Read Sequencing

class CallerName(enum.Enum):
    """Variant caller names"""
    DEEPVARIANT = "DEEPVARIANT"
    GATK = "GATK"
    CLAIR3 = "CLAIR3"

class CallerType(enum.Enum):
    """Variant caller types"""
    ML = "ML"
    TRADITIONAL = "TRADITIONAL"

class TruthSetName(enum.Enum):
    """Benchmarking truth sets"""
    GIAB = "GIAB" # Genome in a Bottle
    CMRG = "CMRG" # Challenging Medically Relevant Genes
    T2T = "T2T" # Telomere to Telomere

class TruthSetReference(enum.Enum):
    """Benchmarking truth set references"""
    GRCH37 = "GRCH37"
    GRCH38 = "GRCH38"

class TruthSetSample(enum.Enum): 
    """Benchmarking samples"""
    HG001 = "HG001"
    HG002 = "HG002"
    HG003 = "HG003"
    HG004 = "HG004"
    HCC1395 = "HCC1395" # triple negative breast cancer

class VariantOrigin(enum.Enum):
    """Origins of variants"""
    GERMLINE = "GERMLINE" # reproductive cells
    SOMATIC = "SOMATIC"  # non-reproductive cells

class VariantSize(enum.Enum):
    """Sizes of variants"""
    SMALL = "SMALL"
    LARGE = "LARGE"

class VariantType(enum.Enum):
    """Types of variants"""
    SNP = "SNP"  # Single Nucleotide Polymorphism
    INDEL = "INDEL" # Insertion/Deletion
    DEL = "DEL"  # Deletion
    INS = "INS"  # Insertion
    SNPINDEL = "SNPINDEL"

class BenchmarkToolName(enum.Enum):
    """Benchmarking tools"""
    HAPPY = "HAPPY"
    VCFDIST = "VCFDIST"
    TRUVARI = "TRUVARI" 

class RegionType(enum.Enum):
    """
    All genomic regions from hap.py stratified analysis.
    Values are kept display-friendly to match hap.py outputs.
    """
    
    # All
    ALL = "All Regions"
    
    # Difficulty
    EASY = "Easy Regions"
    DIFFICULT = "Difficult Regions"
    
    # GC Content
    GC_VERY_LOW = "GC_<15"
    GC_15_20 = "GC_15_20"
    GC_20_25 = "GC_20_25"
    GC_25_30 = "GC_25_30"
    GC_30_55 = "GC_30_55"
    GC_55_60 = "GC_55_60"
    GC_60_65 = "GC_60_65"
    GC_65_70 = "GC_65_70"
    GC_70_75 = "GC_70_75"
    GC_75_80 = "GC_75_80"
    GC_80_85 = "GC_80_85"
    GC_VERY_HIGH = "GC_>85"
    
    # Functional
    REFSEQ_CDS = "RefSeq CDS"
    NOT_IN_CDS = "Non-CDS Regions"
    
    # Repetitive
    SEGDUP = "Segmental Duplications"
    HOMOPOLYMER_4TO6 = "Homopolymer 4-6bp"
    HOMOPOLYMER_7TO11 = "Homopolymer 7-11bp"
    HOMOPOLYMER_GT11 = "Homopolymer >11bp"
    
    # Other
    LOW_MAPPABILITY = "Low Mappability"
    MHC = "MHC Region"
    TS_BOUNDARY = "Truth Set Boundary"
    TS_CONTAINED = "Truth Set Contained"
    
    @classmethod
    def from_string(cls, region_str):
        """
        Convert hap.py region string to enum value.
        
        Maps the raw region strings from hap.py CSV files to their corresponding
        enum values for database storage.
        
        Args:
            region_str (str): Raw region string from hap.py output (e.g., "*", "easy", "GC_<15")
            
        Returns:
            RegionType: Corresponding enum value or None if mapping not found
        """
        mapping = {
            "*": cls.ALL,
            "easy": cls.EASY,
            "difficult": cls.DIFFICULT,
            "GC_<15": cls.GC_VERY_LOW,
            "GC_15_20": cls.GC_15_20,
            "GC_20_25": cls.GC_20_25,
            "GC_25_30": cls.GC_25_30,
            "GC_30_55": cls.GC_30_55,
            "GC_55_60": cls.GC_55_60,
            "GC_60_65": cls.GC_60_65,
            "GC_65_70": cls.GC_65_70,
            "GC_70_75": cls.GC_70_75,
            "GC_75_80": cls.GC_75_80,
            "GC_80_85": cls.GC_80_85,
            "GC_>85": cls.GC_VERY_HIGH,
            "refseq_cds": cls.REFSEQ_CDS,
            "not_in_cds": cls.NOT_IN_CDS,
            "segdup": cls.SEGDUP,
            "homopolymer_4to6": cls.HOMOPOLYMER_4TO6,
            "homopolymer_7to11": cls.HOMOPOLYMER_7TO11,
            "homopolymer_gt11": cls.HOMOPOLYMER_GT11,
            "low_mappability": cls.LOW_MAPPABILITY,
            "MHC": cls.MHC,
            "TS_boundary": cls.TS_BOUNDARY,
            "TS_contained": cls.TS_CONTAINED
        }
        return mapping.get(region_str)
    
    @classmethod
    def from_display_name(cls, display_name):
        """
        Convert UI display names to enum values.
        
        Maps user-friendly region names from the dashboard UI to their
        corresponding enum values for database queries.
        
        Args:
            display_name (str): User-friendly name from UI (e.g., "All Regions", "Easy Regions")
            
        Returns:
            RegionType: Corresponding enum value or None if mapping not found
        """
        mapping = {
            # Main regions
            "All Regions": cls.ALL,
            "Easy Regions": cls.EASY,
            "Difficult Regions": cls.DIFFICULT,
            
            # Functional
            "RefSeq CDS": cls.REFSEQ_CDS,
            "Non-CDS Regions": cls.NOT_IN_CDS,
            
            # Homopolymer
            "Homopolymer 4-6bp": cls.HOMOPOLYMER_4TO6,
            "Homopolymer 7-11bp": cls.HOMOPOLYMER_7TO11,
            "Homopolymer >11bp": cls.HOMOPOLYMER_GT11,
            
            # GC Content (map UI names to enum values)
            "GC_<15": cls.GC_VERY_LOW,
            "GC_15_20": cls.GC_15_20,
            "GC_20_25": cls.GC_20_25,
            "GC_25_30": cls.GC_25_30,
            "GC_30_55": cls.GC_30_55,
            "GC_55_60": cls.GC_55_60,
            "GC_60_65": cls.GC_60_65,
            "GC_65_70": cls.GC_65_70,
            "GC_70_75": cls.GC_70_75,
            "GC_75_80": cls.GC_75_80,
            "GC_80_85": cls.GC_80_85,
            "GC_>85": cls.GC_VERY_HIGH,
            
            # Complex regions  
            "MHC Region": cls.MHC,
            "Segmental Duplications": cls.SEGDUP,
            "Low Mappability": cls.LOW_MAPPABILITY,
        }
        
        return mapping.get(display_name)

# ============================================================================
# DATABASE TABLES
# ============================================================================

class SequencingTechnology(Base):
    """Sequencing technology and platform information"""
    __tablename__ = "sequencing_technologies"

    id = Column(Integer, primary_key=True)
    technology = Column(Enum(SeqTechName), nullable=False)
    target = Column(Enum(SeqTechTarget))
    platform_type = Column(Enum(SeqTechPlatformType))
    platform_name = Column(String(50))
    platform_version = Column(String(50))

    # Relationships 
    experiments = relationship("Experiment", back_populates="sequencing_technology")
    
    def __repr__(self):
        return f"<SequencingTechnology(tech={self.technology.value}, platform={self.platform_name})>"

class VariantCaller(Base): 
    """Variant calling algorithms and details"""
    __tablename__ = 'variant_callers' 

    id = Column(Integer, primary_key=True)
    name = Column(Enum(CallerName), nullable=False)
    type = Column(Enum(CallerType))
    version = Column(String(50))
    model = Column(String(50))

    experiments = relationship("Experiment", back_populates="variant_caller")
    
    def __repr__(self):
        return f"<VariantCaller(name={self.name.value}, version={self.version})>"

class Aligner(Base): 
    """Alignment algorithms and versions"""
    __tablename__ = 'aligners'
    
    id = Column(Integer, primary_key=True)
    name = Column(String(50))
    version = Column(String(50))
    
    experiments = relationship("Experiment", back_populates="aligner")
    
    def __repr__(self):
        return f"<Aligner(name={self.name}, version={self.version})>"

class TruthSet(Base):
    """Validation/Truth sets and details"""
    __tablename__ = 'truth_sets'
    
    id = Column(Integer, primary_key=True)
    name = Column(Enum(TruthSetName))
    version = Column(String(50))
    reference = Column(Enum(TruthSetReference))
    sample = Column(Enum(TruthSetSample))

    experiments = relationship("Experiment", back_populates="truth_set")
    
    def __repr__(self):
        return f"<TruthSet(name={self.name.value}, sample={self.sample.value})>"

class BenchmarkTool(Base):
    """Benchmarking tools and versions"""
    __tablename__ = 'benchmark_tools' 
    
    id = Column(Integer, primary_key=True)
    name = Column(Enum(BenchmarkToolName))
    version = Column(String(50)) 
    
    experiments = relationship("Experiment", back_populates="benchmark_tool")
    
    def __repr__(self):
        return f"<BenchmarkTool(name={self.name.value}, version={self.version})>"

class Variant(Base):
    """Variant types and details"""
    __tablename__ = 'variants'
    
    id = Column(Integer, primary_key=True)
    type = Column(Enum(VariantType))
    size = Column(Enum(VariantSize))
    origin = Column(Enum(VariantOrigin))
    is_phased = Column(Boolean, default=False)
    
    experiments = relationship("Experiment", back_populates="variant")
    
    def __repr__(self):
        return f"<Variant(type={self.type.value}, origin={self.origin.value})>"

class QualityControl(Base):
    """Quality control metrics"""
    __tablename__ = 'quality_control_metrics'

    id = Column(Integer, primary_key=True)
    mean_coverage = Column(Float) 
    read_length = Column(Float) # Only for SRS
    mean_read_length = Column(Float) # Only for LRS
    mean_insert_size = Column(Float) # Only for SRS
    read_quality = Column(Float) 
    max_aligned_read = Column(Float)

    experiments = relationship("Experiment", back_populates="quality_control")
    
    def __repr__(self):
        return f"<QualityControl(coverage={self.mean_coverage}, read_length={self.read_length})>"

class Chemistry(Base):
    """Chemistry details"""
    __tablename__ = 'chemistries'
    id = Column(Integer, primary_key=True)
    name = Column(String(50))  # e.g., "SPRQ"
    version = Column(String(50))
    sequencing_technology = Column(Enum(SeqTechName)) # Related sequencing technology
    sequencing_platform = Column(String(50))

    experiments = relationship("Experiment", back_populates="chemistry")

    def __repr__(self):
        return f"<Chemistry(name={self.name}, version={self.version})>"

# ============================================================================
# MAIN EXPERIMENT TABLE
# ============================================================================

class Experiment(Base):
    """
    Main table linking all metadata and details related to a benchmarking experiment.
    """
    __tablename__ = 'experiments'
    
    id = Column(Integer, primary_key=True)
    name = Column(String(200), nullable=False)
    description = Column(String(1000))
    
    # Foreign keys to reference tables
    sequencing_technology_id = Column(Integer, ForeignKey('sequencing_technologies.id'))
    variant_caller_id = Column(Integer, ForeignKey('variant_callers.id'))
    aligner_id = Column(Integer, ForeignKey('aligners.id'))
    truth_set_id = Column(Integer, ForeignKey('truth_sets.id'))
    benchmark_tool_id = Column(Integer, ForeignKey('benchmark_tools.id'))
    variant_id = Column(Integer, ForeignKey('variants.id'))
    quality_control_metrics_id = Column(Integer, ForeignKey('quality_control_metrics.id'))
    chemistry_id = Column(Integer, ForeignKey('chemistries.id'))
    
    # Timestamps
    created_at = Column(DateTime)

    # Relationships
    sequencing_technology = relationship("SequencingTechnology", back_populates="experiments")
    variant_caller = relationship("VariantCaller", back_populates="experiments")
    aligner = relationship("Aligner", back_populates="experiments")
    truth_set = relationship("TruthSet", back_populates="experiments")
    benchmark_tool = relationship("BenchmarkTool", back_populates="experiments")
    variant = relationship("Variant", back_populates="experiments")
    quality_control = relationship("QualityControl", back_populates="experiments")
    chemistry = relationship("Chemistry", back_populates="experiments")
    benchmark_results = relationship("BenchmarkResult", back_populates="experiment")
    
    def __repr__(self):
        return f"<Experiment(name={self.name})>"

# ============================================================================
# BENCHMARKING RESULTS TABLES
# ============================================================================

class OverallResult(Base):
    """
    Fast access table for overall (*) region results from hap.py files 
    Used for main performane results (Tab 2 and 3) 
    """
    __tablename__ = 'overall_results'
    
    id = Column(Integer, primary_key=True)
    experiment_id = Column(Integer, ForeignKey('experiments.id'), nullable=False)
    
    # Core identifiers
    variant_type = Column(String(20), nullable=False)      # SNP, INDEL
    
    # Performance metrics (most important)
    metric_recall = Column(Float)      
    metric_precision = Column(Float)   
    metric_f1_score = Column(Float)    
    
    # Essential counts
    truth_total = Column(Integer)
    truth_tp = Column(Integer)
    truth_fn = Column(Integer)
    query_total = Column(Integer)
    query_tp = Column(Integer)
    query_fp = Column(Integer)
    
    # Relationship
    experiment = relationship("Experiment")
    
    def __repr__(self):
        return f"<OverallResult(exp_id={self.experiment_id}, type={self.variant_type})>"
    
class BenchmarkResult(Base):
    """
    Full benchmarking experiment results (including all region types) from hap.py output.
    Used for stratified analysis (Tab 4 only)
    """
    __tablename__ = 'benchmark_results'
    
    id = Column(Integer, primary_key=True)
    experiment_id = Column(Integer, ForeignKey('experiments.id'), nullable=False)

    # Core identifiers (filtering criteria)
    variant_type = Column(String(20), nullable=False)      # SNP, INDEL
    subtype = Column(String(100), default='NULL')             # Always 'NULL'
    subset = Column(Enum(RegionType), nullable=False)       # Region types
    filter_type = Column(String(20), default='ALL')       # Always 'ALL'
    
    # Performance metrics
    metric_recall = Column(Float)      # METRIC.Recall
    metric_precision = Column(Float)   # METRIC.Precision
    metric_f1_score = Column(Float)    # METRIC.F1_Score
    
    # Subset information
    subset_size = Column(Float)            # Subset.Size
    subset_is_conf_size = Column(Float)    # Subset.IS_CONF.Size
    
    # Truth set totals
    truth_total = Column(Integer)         # TRUTH.TOTAL
    truth_total_het = Column(Integer)       # TRUTH.TOTAL.het
    truth_total_homalt = Column(Integer)    # TRUTH.TOTAL.homalt
    
    # Truth set true positives
    truth_tp = Column(Integer)          # TRUTH.TP
    truth_tp_het = Column(Integer)        # TRUTH.TP.het
    truth_tp_homalt = Column(Integer)     # TRUTH.TP.homalt
    
    # Truth set false negatives
    truth_fn = Column(Integer)              # TRUTH.FN
    truth_fn_het = Column(Integer)            # TRUTH.FN.het
    truth_fn_homalt = Column(Integer)         # TRUTH.FN.homalt
    
    # Query totals
    query_total = Column(Integer)         # QUERY.TOTAL
    query_total_het = Column(Integer)       # QUERY.TOTAL.het
    query_total_homalt = Column(Integer)    # QUERY.TOTAL.homalt
    
    # Query true positives
    query_tp = Column(Integer)        # QUERY.TP
    query_tp_het = Column(Integer)      # QUERY.TP.het
    query_tp_homalt = Column(Integer)   # QUERY.TP.homalt
    
    # Query false positives
    query_fp = Column(Integer)          # QUERY.FP
    query_fp_het = Column(Integer)        # QUERY.FP.het
    query_fp_homalt = Column(Integer)     # QUERY.FP.homalt
    
    # Query unknown
    query_unk = Column(Integer)           # QUERY.UNK
    query_unk_het = Column(Integer)         # QUERY.UNK.het
    query_unk_homalt = Column(Integer)      # QUERY.UNK.homalt
    
    # Relationship
    experiment = relationship("Experiment", back_populates="benchmark_results")
    
    def __repr__(self):
        return f"<BenchmarkResult(exp_id={self.experiment_id}, type={self.variant_type}, recall={self.metric_recall})>"