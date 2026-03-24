# ============================================================================
# models.py
# ============================================================================
"""
Database models and enums for the Varbench Dashboard (SNV + SV).

Structure:
- Shared enums used by both pipelines
- SNV-specific enums: SNVCallerName, SNVType, RegionType
- SV-specific enums: SVCallerName, SVType, SVSizeRange
- Shared reference tables: users, sequencing_technologies, aligners,
  truth_sets, benchmark_tools, quality_control_metrics, chemistries
- SNV tables: snv_callers, variants, snv_experiments,
  snv_overall_results, snv_benchmark_results
- SV tables: sv_callers, sv_experiments, sv_results
"""

from sqlalchemy import Column, Integer, String, Float, DateTime, Boolean, ForeignKey, Enum, Text, func
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
import enum

Base = declarative_base()

# ============================================================================
# SHARED ENUMS
# ============================================================================

class SeqTechName(enum.Enum):
    ILLUMINA = "ILLUMINA"
    MGI = "MGI"
    ONT = "ONT"
    PACBIO = "PACBIO"
    TENX = "10X"
    ULTIMA = "ULTIMA"

class SeqTechTarget(enum.Enum):
    WGS = "WGS"
    WES = "WES"

class SeqTechPlatformType(enum.Enum):
    SRS = "SRS"       # Short Read Sequencing
    LRS = "LRS"       # Long Read Sequencing
    SYNTHETIC = "SYNTHETIC"

class CallerType(enum.Enum):
    ML = "ML"
    TRADITIONAL = "TRADITIONAL"

class VariantOrigin(enum.Enum):
    GERMLINE = "GERMLINE"
    SOMATIC = "SOMATIC"

class TruthSetName(enum.Enum):
    GIAB = "GIAB"
    CMRG = "CMRG"
    T2T = "T2T"

class TruthSetReference(enum.Enum):
    GRCH37 = "GRCH37"
    GRCH38 = "GRCH38"

class TruthSetSample(enum.Enum):
    HG001 = "HG001"
    HG002 = "HG002"
    HG003 = "HG003"
    HG004 = "HG004"
    HCC1395 = "HCC1395"  # triple negative breast cancer cell line

class BenchmarkToolName(enum.Enum):
    HAPPY = "HAPPY"
    VCFDIST = "VCFDIST"
    TRUVARI = "TRUVARI"

# ============================================================================
# SNV-SPECIFIC ENUMS
# ============================================================================

class SNVCallerName(enum.Enum):
    """SNV variant caller names (hap.py pipeline)"""
    DEEPVARIANT = "DEEPVARIANT"
    GATK3 = "GATK3"
    GATK4 = "GATK4"
    CLAIR3 = "CLAIR3"
    DRAGEN = "DRAGEN"
    LONGRANGER = "LONGRANGER"
    MEGABOLT = "MEGABOLT"
    NANOCALLER = "NANOCALLER"
    PARABRICK = "PARABRICK"
    PEPPER = "PEPPER"

class SNVType(enum.Enum):
    """SNV variant types (hap.py output types)"""
    SNP = "SNP"
    INDEL = "INDEL"
    SNPINDEL = "SNPINDEL"  # combined SNP+INDEL upload

class RegionType(enum.Enum):
    """
    All genomic regions from hap.py stratified analysis.
    Values are display-friendly strings matching hap.py outputs.
    """
    ALL = "All Regions"
    EASY = "Easy Regions"
    DIFFICULT = "Difficult Regions"

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
    GC_LT25_OR_GT65 = "GC <25 or >65"
    GC_LT30_OR_GT55 = "GC <30 or >55"

    REFSEQ_CDS = "RefSeq CDS"
    NOT_IN_CDS = "Non-CDS Regions"

    SEGDUP = "Segmental Duplications"
    NOT_IN_SEGDUPS = "Non-Segmental Duplications"

    HOMOPOLYMER_4TO6 = "Homopolymer 4-6bp"
    HOMOPOLYMER_7TO11 = "Homopolymer 7-11bp"
    HOMOPOLYMER_GT12 = "Homopolymer >12bp"
    HOMOPOLYMER_GE21 = "Homopolymer ≥21bp"
    ALL_TR_AND_HOMOPOLYMERS = "All Tandem Repeats & Homopolymers"
    NOT_IN_ALL_TR_AND_HOMOPOLYMERS = "Non-Tandem Repeats & Non-Homopolymers"

    SATELLITES = "Satellites"
    NOT_IN_SATELLITES = "Non-Satellites"

    LOW_MAPPABILITY = "Low Mappability"
    NOT_IN_LOW_MAPPABILITY = "Non-Low Mappability"

    MHC = "MHC Region"
    TS_BOUNDARY = "Truth Set Boundary"
    TS_CONTAINED = "Truth Set Contained"

    @classmethod
    def from_string(cls, region_str):
        """Map raw hap.py region string to enum (case-insensitive). Returns None if unknown."""
        if not region_str:
            return None

        region_str_lower = str(region_str).strip().lower()

        mapping = {
            # Core regions
            "*": cls.ALL,
            "easy": cls.EASY,
            "difficult": cls.DIFFICULT,

            # GC Content

            "gc_<15": cls.GC_VERY_LOW,
            "gc_15_20": cls.GC_15_20,
            "gc_20_25": cls.GC_20_25,
            "gc_25_30": cls.GC_25_30,
            "gc_30_55": cls.GC_30_55,
            "gc_55_60": cls.GC_55_60,
            "gc_60_65": cls.GC_60_65,
            "gc_65_70": cls.GC_65_70,
            "gc_70_75": cls.GC_70_75,
            "gc_75_80": cls.GC_75_80,
            "gc_80_85": cls.GC_80_85,
            "gc_>85": cls.GC_VERY_HIGH,

            "gc15": cls.GC_VERY_LOW,
            "gc15to20": cls.GC_15_20,
            "gc20to25": cls.GC_20_25,
            "gc25to30": cls.GC_25_30,
            "gc30to55": cls.GC_30_55,
            "gc55to60": cls.GC_55_60,
            "gc60to65": cls.GC_60_65,
            "gc65to70": cls.GC_65_70,
            "gc70to75": cls.GC_70_75,
            "gc75to80": cls.GC_75_80,
            "gc80to85": cls.GC_80_85,
            "gc85": cls.GC_VERY_HIGH,

            # Extreme GC ranges
            "gclt25orgt65": cls.GC_LT25_OR_GT65,
            "gclt30orgt55": cls.GC_LT30_OR_GT55,

            # Functional regions
            "refseq_cds": cls.REFSEQ_CDS,
            "not_in_cds": cls.NOT_IN_CDS,
            "not_in_refseq_cds": cls.NOT_IN_CDS,

            # Segmental duplications
            "segdup": cls.SEGDUP,
            "segdups": cls.SEGDUP,
            "not_in_segdups": cls.NOT_IN_SEGDUPS,
            
            # Homopolymers
            "homopolymer_4to6": cls.HOMOPOLYMER_4TO6,
            "homopolymer_7to11": cls.HOMOPOLYMER_7TO11,
            "homopolymer_gt11": cls.HOMOPOLYMER_GT12,
            "homopolymer_gt12": cls.HOMOPOLYMER_GT12,
            "homopolymer_ge12": cls.HOMOPOLYMER_GT12,
            "homopolymer_ge21": cls.HOMOPOLYMER_GE21,
            
            # Tandem repeats & homopolymers
            "all_tr_and_homopolymers": cls.ALL_TR_AND_HOMOPOLYMERS,
            "not_in_all_tr_and_homopolymers": cls.NOT_IN_ALL_TR_AND_HOMOPOLYMERS,
            
            # Satellites
            "satellites": cls.SATELLITES,
            "not_in_satellites": cls.NOT_IN_SATELLITES,
            
            # Mappability
            "low_mappability": cls.LOW_MAPPABILITY,
            "not_in_low_mappability": cls.NOT_IN_LOW_MAPPABILITY,
            
            # Special regions
            "mhc": cls.MHC,
            "ts_boundary": cls.TS_BOUNDARY,
            "ts_contained": cls.TS_CONTAINED
        }
        return mapping.get(region_str_lower)

    @classmethod
    def from_display_name(cls, display_name):
        """Reverse-lookup by display value string. OPTIONAL"""
        if not display_name:
            return None
        for member in cls:
            if member.value.lower() == str(display_name).strip().lower():
                return member
        return None

# ============================================================================
# SV-SPECIFIC ENUMS
# ============================================================================

class SVCallerName(enum.Enum):
    """SV variant caller names (Truvari pipeline)"""
    METASV = "METASV"
    DYSGU = "DYSGU"
    CUTESV = "CUTESV"
    SNIFFLES = "SNIFFLES"
    SVIM = "SVIM"
    PDSV = "PDSV"
    SAWFISH = "SAWFISH"

class SVType(enum.Enum):
    """SV types from Truvari directory naming"""
    DEL = "DEL"
    INS = "INS"

class SVSizeRange(enum.Enum):
    """SV size bins from Truvari directory naming"""
    ALL = "ALL"
    SIZE_30_50 = "30_50"
    SIZE_50_250 = "50_250"
    SIZE_250_500 = "250_500"
    SIZE_500_6000 = "500_6000"
    SIZE_6000PLUS = "6000plus"

# ============================================================================
# SHARED REFERENCE TABLES
# ============================================================================

class User(Base):
    """OIDC-sourced users; is_admin from COManage group membership"""
    __tablename__ = 'users'

    id = Column(Integer, primary_key=True)
    username = Column(String(100), unique=True, nullable=False)
    email = Column(String(200))
    full_name = Column(String(200))
    is_admin = Column(Boolean, default=False)
    created_at = Column(DateTime)
    last_login = Column(DateTime)

    snv_experiments = relationship("SNVExperiment", back_populates="owner")
    sv_experiments = relationship("SVExperiment", back_populates="owner")

    def __repr__(self):
        return f"<User(username={self.username}, admin={self.is_admin})>"


class SequencingTechnology(Base):
    """Sequencing platform metadata"""
    __tablename__ = 'sequencing_technologies'

    id = Column(Integer, primary_key=True)
    technology = Column(Enum(SeqTechName))
    target = Column(Enum(SeqTechTarget))
    platform_type = Column(Enum(SeqTechPlatformType))
    platform_name = Column(String(100))
    platform_version = Column(Float)

    # Relationships
    snv_experiments = relationship("SNVExperiment", back_populates="sequencing_technology")
    sv_experiments = relationship("SVExperiment", back_populates="sequencing_technology")

    def __repr__(self):
        return f"<SequencingTechnology(technology={self.technology.value}, platform={self.platform_name})>"


class Aligner(Base):
    """Aligners — free text, no enum (new aligners added automatically)"""
    __tablename__ = 'aligners'

    id = Column(Integer, primary_key=True)
    name = Column(String(50))
    version = Column(String(50))

    snv_experiments = relationship("SNVExperiment", back_populates="aligner")
    sv_experiments = relationship("SVExperiment", back_populates="aligner")

    def __repr__(self):
        return f"<Aligner(name={self.name}, version={self.version})>"


class TruthSet(Base):
    """Validation truth sets"""
    __tablename__ = 'truth_sets'

    id = Column(Integer, primary_key=True)
    name = Column(Enum(TruthSetName))
    version = Column(String(50))
    reference = Column(Enum(TruthSetReference))
    sample = Column(Enum(TruthSetSample))

    snv_experiments = relationship("SNVExperiment", back_populates="truth_set")
    sv_experiments = relationship("SVExperiment", back_populates="truth_set")

    def __repr__(self):
        return f"<TruthSet(name={self.name.value}, sample={self.sample.value})>"


class BenchmarkTool(Base):
    """Benchmarking tools and versions"""
    __tablename__ = 'benchmark_tools'

    id = Column(Integer, primary_key=True)
    name = Column(Enum(BenchmarkToolName))
    version = Column(String(50))

    snv_experiments = relationship("SNVExperiment", back_populates="benchmark_tool")
    sv_experiments = relationship("SVExperiment", back_populates="benchmark_tool")

    def __repr__(self):
        return f"<BenchmarkTool(name={self.name.value}, version={self.version})>"


class QualityControl(Base):
    """QC metrics; read_length for SRS only, mean_read_length for LRS only"""
    __tablename__ = 'quality_control_metrics'

    id = Column(Integer, primary_key=True)
    mean_coverage = Column(Float)
    read_length = Column(Float)         # SRS only
    mean_read_length = Column(Float)    # LRS only
    mean_insert_size = Column(Float)    # SRS only
    read_quality = Column(Float)
    max_aligned_read = Column(Float)

    snv_experiments = relationship("SNVExperiment", back_populates="quality_control")
    sv_experiments = relationship("SVExperiment", back_populates="quality_control")

    def __repr__(self):
        return f"<QualityControl(coverage={self.mean_coverage})>"


class Chemistry(Base):
    """Library chemistry (e.g., SPRQ for PacBio Revio)"""
    __tablename__ = 'chemistries'

    id = Column(Integer, primary_key=True)
    name = Column(String(50))
    version = Column(String(50))
    sequencing_technology = Column(Enum(SeqTechName))
    sequencing_platform = Column(String(50))

    snv_experiments = relationship("SNVExperiment", back_populates="chemistry")
    sv_experiments = relationship("SVExperiment", back_populates="chemistry")

    def __repr__(self):
        return f"<Chemistry(name={self.name}, version={self.version})>"

# ============================================================================
# SNV TABLES
# ============================================================================

class SNVCaller(Base):
    """SNV variant callers — separate from SV callers"""
    __tablename__ = 'snv_callers'

    id = Column(Integer, primary_key=True)
    name = Column(Enum(SNVCallerName))
    type = Column(Enum(CallerType))
    version = Column(String(50))
    model = Column(String(100))

    snv_experiments = relationship("SNVExperiment", back_populates="snv_caller")

    def __repr__(self):
        return f"<SNVCaller(name={self.name.value}, version={self.version})>"


class SNVVariant(Base):
    """SNV-specific variant metadata. type/origin/is_phased describe the SNV upload target."""
    __tablename__ = 'snv_variants'

    id = Column(Integer, primary_key=True)
    type = Column(Enum(SNVType))        # SNP, INDEL, or SNPINDEL
    origin = Column(Enum(VariantOrigin))    # GERMLINE or SOMATIC
    is_phased = Column(Boolean, default=False)

    snv_experiments = relationship("SNVExperiment", back_populates="snv_variant")

    def __repr__(self):
        return f"<SNVVariant(type={self.type.value}, origin={self.origin.value})>"


class SNVExperiment(Base):
    """One row per hap.py upload"""
    __tablename__ = 'snv_experiments'

    id = Column(Integer, primary_key=True)
    name = Column(String(200), nullable=False)
    description = Column(String(1000))
    is_public = Column(Boolean, default=True)
    created_by_username = Column(String(100))
    created_at = Column(DateTime)

    owner_id = Column(Integer, ForeignKey('users.id'), nullable=True) # NULL = public legacy data
    sequencing_technology_id = Column(Integer, ForeignKey('sequencing_technologies.id'))
    snv_caller_id = Column(Integer, ForeignKey('snv_callers.id'))
    aligner_id = Column(Integer, ForeignKey('aligners.id'))
    truth_set_id = Column(Integer, ForeignKey('truth_sets.id'))
    benchmark_tool_id = Column(Integer, ForeignKey('benchmark_tools.id'))
    snv_variant_id = Column(Integer, ForeignKey('snv_variants.id'))
    quality_control_metrics_id = Column(Integer, ForeignKey('quality_control_metrics.id'))
    chemistry_id = Column(Integer, ForeignKey('chemistries.id'))

    # Relationships
    owner = relationship("User", back_populates="snv_experiments")
    sequencing_technology = relationship("SequencingTechnology", back_populates="snv_experiments")
    snv_caller = relationship("SNVCaller", back_populates="snv_experiments")
    aligner = relationship("Aligner", back_populates="snv_experiments")
    truth_set = relationship("TruthSet", back_populates="snv_experiments")
    benchmark_tool = relationship("BenchmarkTool", back_populates="snv_experiments")
    snv_variant = relationship("SNVVariant", back_populates="snv_experiments")
    quality_control = relationship("QualityControl", back_populates="snv_experiments")
    chemistry = relationship("Chemistry", back_populates="snv_experiments")
    snv_overall_results = relationship("SNVOverallResult", back_populates="experiment")
    snv_benchmark_results = relationship("SNVBenchmarkResult", back_populates="experiment")

    def __repr__(self):
        return f"<SNVExperiment(name={self.name})>"


class SNVOverallResult(Base):
    """
    Fast-access table for overall (*) region results from hap.py.
    One row per (experiment, variant_type). Used for Tabs 2 and 3.
    """
    __tablename__ = 'snv_overall_results'

    id = Column(Integer, primary_key=True)
    experiment_id = Column(Integer, ForeignKey('snv_experiments.id'), nullable=False)
    variant_type = Column(String(20), nullable=False)   # SNP or INDEL

    metric_recall = Column(Float)
    metric_precision = Column(Float)
    metric_f1_score = Column(Float)

    truth_total = Column(Integer)
    truth_tp = Column(Integer)
    truth_fn = Column(Integer)
    query_total = Column(Integer)
    query_tp = Column(Integer)
    query_fp = Column(Integer)

    experiment = relationship("SNVExperiment", back_populates="snv_overall_results")

    def __repr__(self):
        return f"<SNVOverallResult(exp_id={self.experiment_id}, type={self.variant_type})>"


class SNVBenchmarkResult(Base):
    """
    Full stratified hap.py results including all region types.
    One row per (experiment, variant_type, region). Used for Tab 4.
    """
    __tablename__ = 'snv_benchmark_results'

    id = Column(Integer, primary_key=True)
    experiment_id = Column(Integer, ForeignKey('snv_experiments.id'), nullable=False)

    variant_type = Column(String(20), nullable=False)
    subtype = Column(String(100), default='NULL')
    subset = Column(Enum(RegionType), nullable=False)
    filter_type = Column(String(20), default='ALL')

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

    experiment = relationship("SNVExperiment", back_populates="snv_benchmark_results")

    def __repr__(self):
        return f"<SNVBenchmarkResult(exp_id={self.experiment_id}, type={self.variant_type}, recall={self.metric_recall})>"

# ============================================================================
# SV TABLES
# ============================================================================

class SVCaller(Base):
    """SV variant callers — separate from SNV callers"""
    __tablename__ = 'sv_callers'

    id = Column(Integer, primary_key=True)
    name = Column(Enum(SVCallerName))
    type = Column(Enum(CallerType))
    version = Column(String(50))
    model = Column(String(100))

    sv_experiments = relationship("SVExperiment", back_populates="sv_caller")

    def __repr__(self):
        return f"<SVCaller(name={self.name.value}, version={self.version})>"

class SVVariant(Base):
    """SV-specific variant metadata."""
    __tablename__ = 'sv_variants'

    id = Column(Integer, primary_key=True)
    type = Column(Enum(SVType))
    origin = Column(Enum(VariantOrigin))
    is_phased = Column(Boolean, default=False)
    size_bin = Column(Enum(SVSizeRange))   # ALL means no size filter applied

    sv_experiments = relationship("SVExperiment", back_populates="sv_variant")

    def __repr__(self):
        return f"<SVVariant(type={self.type.value}, origin={self.origin.value}, size={self.size_bin.value})>"
    
class SVResultType(enum.Enum):
    """ Refind vs Summary"""
    SUMMARY  = "SUMMARY"   # from summary.json — all truth sets
    REFINED = "REFINED"  # from refine.variant_summary.json — T2T only

class SVExperiment(Base):
    """
    One row per uploaded parent Truvari directory.
    """
    __tablename__ = 'sv_experiments'

    id = Column(Integer, primary_key=True)
    name = Column(String(200), nullable=False)
    description = Column(String(1000))
    is_public = Column(Boolean, default=True)
    created_by_username = Column(String(100))
    created_at = Column(DateTime)

    owner_id = Column(Integer, ForeignKey('users.id'), nullable=True)
    sequencing_technology_id = Column(Integer, ForeignKey('sequencing_technologies.id'))
    sv_caller_id = Column(Integer, ForeignKey('sv_callers.id'))
    sv_variant_id = Column(Integer, ForeignKey('sv_variants.id'))
    aligner_id = Column(Integer, ForeignKey('aligners.id'))
    truth_set_id = Column(Integer, ForeignKey('truth_sets.id'))
    benchmark_tool_id = Column(Integer, ForeignKey('benchmark_tools.id'))
    quality_control_metrics_id = Column(Integer, ForeignKey('quality_control_metrics.id'))
    chemistry_id = Column(Integer, ForeignKey('chemistries.id'))

    owner = relationship("User", back_populates="sv_experiments")
    sequencing_technology = relationship("SequencingTechnology", back_populates="sv_experiments")
    sv_caller = relationship("SVCaller", back_populates="sv_experiments")
    aligner = relationship("Aligner", back_populates="sv_experiments")
    truth_set = relationship("TruthSet", back_populates="sv_experiments")
    benchmark_tool = relationship("BenchmarkTool", back_populates="sv_experiments")
    quality_control = relationship("QualityControl", back_populates="sv_experiments")
    chemistry = relationship("Chemistry", back_populates="sv_experiments")
    sv_variant = relationship("SVVariant", back_populates="sv_experiments")
    sv_results = relationship("SVResult", back_populates="experiment")

    def __repr__(self):
        return f"<SVExperiment(name={self.name})>"


class SVResult(Base):
    """
    One row per JSON file parsed from a Truvari subdir.
    One experiment yields 1 row per (sv_type, size_range) for GIAB/CMRG,
    and 2 rows for T2T — one SUMMARY and one REFINED.
    """
    __tablename__ = 'sv_results'

    id = Column(Integer, primary_key=True)
    experiment_id = Column(Integer, ForeignKey('sv_experiments.id'), nullable=False)

    sv_type = Column(Enum(SVType), nullable=False)
    size_range = Column(Enum(SVSizeRange), nullable=False)
    result_type = Column(Enum(SVResultType), nullable=False)  # unique key: (experiment_id, sv_type, size_range, result_type)

    metric_recall = Column(Float)
    metric_precision = Column(Float)
    metric_f1_score = Column(Float)

    base_count = Column(Integer)     # truth total (base cnt)
    comp_count = Column(Integer)     # call total (comp cnt)
    truth_tp = Column(Integer)       # TP-base
    call_tp = Column(Integer)        # TP-comp
    fp = Column(Integer)
    fn = Column(Integer)

    gt_concordance = Column(Float)   # NULL for REFINED rows
    gt_matrix_json = Column(Text)    # NULL for REFINED rows; stored as JSON string

    tp_comp_tp_gt = Column(Integer)   # TP-comp_TP-gt — NULL for REFINED
    tp_comp_fp_gt = Column(Integer)   # TP-comp_FP-gt — NULL for REFINED
    tp_base_tp_gt = Column(Integer)   # TP-base_TP-gt — NULL for REFINED
    tp_base_fp_gt = Column(Integer)   # TP-base_FP-gt — NULL for REFINED

    experiment = relationship("SVExperiment", back_populates="sv_results")

    def __repr__(self):
        return f"<SVResult(exp_id={self.experiment_id}, type={self.sv_type.value}, size={self.size_range.value}, result={self.result_type.value})>"