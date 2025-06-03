# Defining normalized database tables for each type of metadata using SQLAlchemy

from sqlalchemy import Column, Integer, String, Float, DateTime, Boolean, ForeignKey, Enum, func
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
import enum

Base = declarative_base()

# ============================================================================
# ENUM DEFINITIONS
# ============================================================================

class SeqTechName(enum.Enum):
    """Sequencing technologies"""
    ILLUMINA = "Illumina"
    MGI = "MGI"
    ONT = "ONT"
    PACBIO = "PacBio"

class SeqTechTarget(enum.Enum):
    """ Sequencing targets """
    WGS = "WGS" # whole genome sequencing
    WES = "WES" # exon sequencing    

class SeqTechPlatformType(enum.Enum):
    """ Sequencing Platform types """
    SRS = "SRS"  # Short Read Sequencing
    LRS = "LRS" # Long Read Sequencing

class CallerName(enum.Enum):
    """ Variant caller names """
    DEEPVARIANT = "DeepVariant"
    GATK = "GATK"
    CLAIR3 = "Clair3"

class CallerType(enum.Enum):
    """ Variant caller types"""
    ML = "ML"
    TRADITIONAL = "Traditional"

class TruthSetName(enum.Enum):
    """ Benchmarking truth sets """
    GIAB = "GIAB" # Genome in a Bottle
    CMRG = "CMRG" # Challenging Medically Relevant Genes
    T2T = "T2T" # Telomere to Telomere

class TruthSetReference(enum.Enum):
    """ Benchmarking truth set references"""
    GRCH37 = "GRCh37"
    GRCH38 = "GRCh38"

class TruthSetSample(enum.Enum): 
    """ Benchmarking samples """
    HG001 = "HG001"
    HG002 = "HG002"
    HG003 = "HG003"
    HG004 = "HG004"
    HCC1395 = "HCC1395" # triple negative breast cancer

class VariantOrigin(enum.Enum):
    """ Origins of variants """
    GERMLINE = "Germline" # reproductive cells
    SOMATIC = "Somatic"  # non-productive cells

class VariantSize(enum.Enum):
    """ Sizes of variants """
    SMALL = "Small"
    LARGE = "Large"

class VariantType(enum.Enum):
    """ Types of variants """
    SNP = "SNP"  # Single Nucleotide Polymorphism
    INDEL = "INDEL" # Insertion/Deletion
    DEL = "DEL"  # Deletion
    INS = "INS"  # Insertion
    SNPINDEL = "SNP+INDEL" #-----------------------------------------------------------------------------------------------------

class BenchmarkToolName(enum.Enum):
    """benchmarking tools"""
    HAPPY = "hap.py"
    VCFDIST = "vcfdist"
    TRUVARI = "truvari" 

    
# ============================================================================
# DATABASE TABLES
# ============================================================================

class SequencingTechnology(Base):
    """
    Sequencing technology and platform information.
    """
    __tablename__ = "sequencing_technologies"

    id = Column(Integer, primary_key=True)
    technology = Column(Enum(SeqTechName), nullable=False)
    target = Column(Enum(SeqTechTarget))
    platform_type = Column(Enum(SeqTechPlatformType))
    platform_name = Column(String(50))
    platform_version = Column(String(50))

    # Relationships 
    experiments = relationship("Experiment", back_populates="sequencing_technology")  
    # in the 'one' class: 
    # 'many' class table title = relationship("'many' class python name", back_populates="field name in 'many'class"-----------------------####
    
    def __repr__(self):
        return f"<SequencingTechnology(tech={self.technology.value}, platform={self.platform_name})>"


class VariantCaller(Base): 
    """
    Variant calling algorithms and details.
    """
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
    """
    Alignment algorithms and versions.
    """
    __tablename__ = 'aligners'
    
    id = Column(Integer, primary_key=True)
    name = Column(String(50))
    version = Column(String(50))
    
    experiments = relationship("Experiment", back_populates="aligner")
    
    def __repr__(self):
        return f"<Aligner(name={self.name}, version={self.version})>"

class TruthSet(Base):
    """ 
    Validation/Truth sets and details.
    """
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
    """
    Benchmarking tools and versions
    """
    __tablename__ = 'benchmark_tools' 
    
    id = Column(Integer, primary_key=True)
    name = Column(Enum(BenchmarkToolName))
    version = Column(String(50)) 
    
    experiments = relationship("Experiment", back_populates="benchmark_tool")
    
    def __repr__(self):
        return f"<BenchmarkTool(name={self.name.value}, version={self.version})>"

class Variant(Base):
    """ 
    Variant types and details.
    """
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
    """
    Quality control metrics.
    """
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
    """
    Chemistry details.
    """
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
    Main table linking all the metadata adn details related to a benchmarking experiment.
    """
    __tablename__ = 'experiments'
    
    id = Column(Integer, primary_key=True)
    name = Column(String(200), nullable=False)
    description = Column(String(1000))
    
    # Foreign keys to reference tables
    sequencing_technology_id = Column(Integer, ForeignKey('sequencing_technologies.id')) #referencing the table name
    variant_caller_id = Column(Integer, ForeignKey('variant_callers.id'))
    aligner_id = Column(Integer, ForeignKey('aligners.id'))
    truth_set_id = Column(Integer, ForeignKey('truth_sets.id'))
    benchmark_tool_id = Column(Integer, ForeignKey('benchmark_tools.id'))
    variant_id = Column(Integer, ForeignKey('variants.id'))
    quality_control_metrics_id = Column(Integer, ForeignKey('quality_control_metrics.id'))
    chemistry_id = Column(Integer, ForeignKey('chemistries.id'))
    
    # Timestamps
    created_at = Column(DateTime, default=func.now())

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
# BENCHMARKING RESULTS TABLE
# ============================================================================

class BenchmarkResult(Base):
    """
    Benchmarking experiment results.
    """
    __tablename__ = 'benchmark_results'
    id = Column(Integer, primary_key=True)
    experiment_id = Column(Integer, ForeignKey('experiments.id'))
    
    # Core columns
    type = Column(String(20))           # SNP, INDEL
    subset = Column(String(100))        # *, easy, difficult
    filter = Column(String(20))         # PASS, ALL
    
    # Performance metrics
    recall = Column(Float)              # METRIC.Recall
    precision = Column(Float)           # METRIC.Precision  
    f1_score = Column(Float)           # METRIC.F1_Score
    
    # Count data
    truth_total = Column(Integer)       # TRUTH.TOTAL
    truth_tp = Column(Integer)         # TRUTH.TP
    truth_fn = Column(Integer)         # TRUTH.FN
    query_tp = Column(Integer)         # QUERY.TP
    query_fp = Column(Integer)         # QUERY.FP
    query_unk = Column(Integer)        # QUERY.UNK
    
    # Relationship
    experiment = relationship("Experiment", back_populates="benchmark_results")
# ============================================================================
# OTHER FUNCTIONS
# ============================================================================
'''
def get_enum_values(enum_class):
    """Get all possible values for an enum"""
    return [e.value for e in enum_class]


'''