# ============================================================================
# user_models.py
# ============================================================================
"""
User-specific database models for SNV Benchmarking Dashboard.
These models handle user authentication and user-specific experiments.
Same structure as the models for public experiments, but private to each user.
"""

from sqlalchemy import Column, Integer, String, Float, DateTime, Boolean, ForeignKey, Enum, func, Text
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
import enum
import hashlib
import secrets
from models import Base  

# ============================================================================
# USER AUTHENTICATION MODELS
# ============================================================================

class UserRole(enum.Enum):
    """User role definitions"""
    USER = "user" # Regular user - can upload their own experiments
    ADMIN = "admin" # Admin user - can manage public experiments and other users

class User(Base):
    """
    User accounts with authentication and role management.
    Separate from public experiments.
    """
    __tablename__ = 'users'
    
    id = Column(Integer, primary_key=True)
    username = Column(String(50), unique=True, nullable=False, index=True)
    email = Column(String(100), unique=True, nullable=False, index=True)
    password_hash = Column(String(255), nullable=False)
    role = Column(Enum(UserRole), default=UserRole.USER, nullable=False)
    created_at = Column(DateTime, default=func.now())
    # last_login = Column(DateTime)
    is_active = Column(Boolean, default=True)
    
    # Relationships
    experiments = relationship("UserExperiment", back_populates="user", cascade="all, delete-orphan") # experiments get deleted by user 
    
    def set_password(self, password):
        """Hash and store password securely"""
        salt = secrets.token_hex(16)
        password_hash = hashlib.sha256((password + salt).encode()).hexdigest()
        self.password_hash = f"{salt}:{password_hash}"
    
    def check_password(self, password):
        """Verify password against stored hash"""
        try:
            salt, stored_hash = self.password_hash.split(':')
            test_hash = hashlib.sha256((password + salt).encode()).hexdigest()
            return test_hash == stored_hash
        except:
            return False
    
    def is_admin(self):
        """Check if user has admin privileges"""
        return self.role == UserRole.ADMIN
    
    def __repr__(self):
        return f"<User(username={self.username}, role={self.role.value})>"

# ============================================================================
# USER EXPERIMENT MODELS
# ============================================================================

class UserSequencingTechnology(Base):
    """User-specific sequencing technology metadata"""
    __tablename__ = "user_sequencing_technologies"
    
    id = Column(Integer, primary_key=True)
    technology = Column(String(50), nullable=False)  # Store as string for flexibility --------------------------
    target = Column(String(50))
    platform_type = Column(String(50))
    platform_name = Column(String(50))
    platform_version = Column(String(50))
    
    experiments = relationship("UserExperiment", back_populates="sequencing_technology")
    
    def __repr__(self):
        return f"<UserSequencingTechnology(tech={self.technology}, platform={self.platform_name})>"

class UserVariantCaller(Base):
    """User-specific variant caller metadata"""
    __tablename__ = 'user_variant_callers'
    
    id = Column(Integer, primary_key=True)
    name = Column(String(50), nullable=False)
    type = Column(String(50))
    version = Column(String(50))
    model = Column(String(50))
    
    experiments = relationship("UserExperiment", back_populates="variant_caller")
    
    def __repr__(self):
        return f"<UserVariantCaller(name={self.name}, version={self.version})>"

class UserAligner(Base):
    """User-specific aligner metadata"""
    __tablename__ = 'user_aligners'
    
    id = Column(Integer, primary_key=True)
    name = Column(String(50))
    version = Column(String(50))
    
    experiments = relationship("UserExperiment", back_populates="aligner")
    
    def __repr__(self):
        return f"<UserAligner(name={self.name}, version={self.version})>"

class UserTruthSet(Base):
    """User-specific truth set metadata"""
    __tablename__ = 'user_truth_sets'
    
    id = Column(Integer, primary_key=True)
    name = Column(String(50))
    version = Column(String(50))
    reference = Column(String(50))
    sample = Column(String(50))
    
    experiments = relationship("UserExperiment", back_populates="truth_set")
    
    def __repr__(self):
        return f"<UserTruthSet(name={self.name}, sample={self.sample})>"

class UserBenchmarkTool(Base):
    """User-specific benchmark tool metadata"""
    __tablename__ = 'user_benchmark_tools'
    
    id = Column(Integer, primary_key=True)
    name = Column(String(50))
    version = Column(String(50))
    
    experiments = relationship("UserExperiment", back_populates="benchmark_tool")
    
    def __repr__(self):
        return f"<UserBenchmarkTool(name={self.name}, version={self.version})>"

class UserVariant(Base):
    """User-specific variant metadata"""
    __tablename__ = 'user_variants'
    
    id = Column(Integer, primary_key=True)
    type = Column(String(50))
    size = Column(String(50))
    origin = Column(String(50))
    is_phased = Column(Boolean, default=False)
    
    experiments = relationship("UserExperiment", back_populates="variant")
    
    def __repr__(self):
        return f"<UserVariant(type={self.type}, origin={self.origin})>"

class UserQualityControl(Base):
    """User-specific quality control metrics"""
    __tablename__ = 'user_quality_control_metrics'
    
    id = Column(Integer, primary_key=True)
    mean_coverage = Column(Float)
    read_length = Column(Float)
    mean_read_length = Column(Float)
    mean_insert_size = Column(Float)
    read_quality = Column(Float)
    max_aligned_read = Column(Float)
    
    experiments = relationship("UserExperiment", back_populates="quality_control")
    
    def __repr__(self):
        return f"<UserQualityControl(coverage={self.mean_coverage})>"

class UserChemistry(Base):
    """User-specific chemistry metadata"""
    __tablename__ = 'user_chemistries'
    
    id = Column(Integer, primary_key=True)
    name = Column(String(50))
    version = Column(String(50))
    sequencing_technology = Column(String(50))
    sequencing_platform = Column(String(50))
    
    experiments = relationship("UserExperiment", back_populates="chemistry")
    
    def __repr__(self):
        return f"<UserChemistry(name={self.name}, version={self.version})>"

# ============================================================================
# MAIN USER EXPERIMENT TABLE
# ============================================================================

class UserExperiment(Base):
    """
    User-specific experiments - mirrors the public Experiment table structure
    but allows users to upload and manage their own data
    """
    __tablename__ = 'user_experiments'
    
    id = Column(Integer, primary_key=True)
    user_id = Column(Integer, ForeignKey('users.id'), nullable=False, index=True)
    name = Column(String(200), nullable=False)
    description = Column(Text)
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())
    
    # Foreign keys to user-specific metadata tables
    sequencing_technology_id = Column(Integer, ForeignKey('user_sequencing_technologies.id'))
    variant_caller_id = Column(Integer, ForeignKey('user_variant_callers.id'))
    aligner_id = Column(Integer, ForeignKey('user_aligners.id'))
    truth_set_id = Column(Integer, ForeignKey('user_truth_sets.id'))
    benchmark_tool_id = Column(Integer, ForeignKey('user_benchmark_tools.id'))
    variant_id = Column(Integer, ForeignKey('user_variants.id'))
    quality_control_metrics_id = Column(Integer, ForeignKey('user_quality_control_metrics.id'))
    chemistry_id = Column(Integer, ForeignKey('user_chemistries.id'))
    
    # File tracking
    original_filename = Column(String(255))
    file_size = Column(Integer)
    
    # Relationships
    user = relationship("User", back_populates="experiments")
    sequencing_technology = relationship("UserSequencingTechnology", back_populates="experiments")
    variant_caller = relationship("UserVariantCaller", back_populates="experiments")
    aligner = relationship("UserAligner", back_populates="experiments")
    truth_set = relationship("UserTruthSet", back_populates="experiments")
    benchmark_tool = relationship("UserBenchmarkTool", back_populates="experiments")
    variant = relationship("UserVariant", back_populates="experiments")
    quality_control = relationship("UserQualityControl", back_populates="experiments")
    chemistry = relationship("UserChemistry", back_populates="experiments")
    
    # Results relationships
    benchmark_results = relationship("UserBenchmarkResult", back_populates="experiment", cascade="all, delete-orphan")
    overall_results = relationship("UserOverallResult", back_populates="experiment", cascade="all, delete-orphan")
    
    def __repr__(self):
        return f"<UserExperiment(id={self.id}, user_id={self.user_id}, name={self.name})>"

# ============================================================================
# USER RESULTS TABLES
# ============================================================================

class UserOverallResult(Base):
    """User-specific overall results - mirrors OverallResult table"""
    __tablename__ = 'user_overall_results'
    
    id = Column(Integer, primary_key=True)
    experiment_id = Column(Integer, ForeignKey('user_experiments.id'), nullable=False, index=True)
    
    # Core identifiers
    variant_type = Column(String(20), nullable=False)
    
    # Performance metrics
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
    experiment = relationship("UserExperiment", back_populates="overall_results")
    
    def __repr__(self):
        return f"<UserOverallResult(exp_id={self.experiment_id}, type={self.variant_type})>"

class UserBenchmarkResult(Base):
    """User-specific detailed benchmark results - mirrors BenchmarkResult table"""
    __tablename__ = 'user_benchmark_results'
    
    id = Column(Integer, primary_key=True)
    experiment_id = Column(Integer, ForeignKey('user_experiments.id'), nullable=False, index=True)
    
    # Core identifiers
    variant_type = Column(String(20), nullable=False)
    subtype = Column(String(100), default='NULL')
    subset = Column(String(100), nullable=False)  # Region type as string for flexibility
    filter_type = Column(String(20), default='ALL')
    
    # Performance metrics
    metric_recall = Column(Float)
    metric_precision = Column(Float)
    metric_f1_score = Column(Float)
    
    # Subset information
    subset_size = Column(Float)
    subset_is_conf_size = Column(Float)
    
    # Truth set totals
    truth_total = Column(Integer)
    truth_total_het = Column(Integer)
    truth_total_homalt = Column(Integer)
    
    # Truth set true positives
    truth_tp = Column(Integer)
    truth_tp_het = Column(Integer)
    truth_tp_homalt = Column(Integer)
    
    # Truth set false negatives
    truth_fn = Column(Integer)
    truth_fn_het = Column(Integer)
    truth_fn_homalt = Column(Integer)
    
    # Query totals
    query_total = Column(Integer)
    query_total_het = Column(Integer)
    query_total_homalt = Column(Integer)
    
    # Query true positives
    query_tp = Column(Integer)
    query_tp_het = Column(Integer)
    query_tp_homalt = Column(Integer)
    
    # Query false positives
    query_fp = Column(Integer)
    query_fp_het = Column(Integer)
    query_fp_homalt = Column(Integer)
    
    # Query unknown
    query_unk = Column(Integer)
    query_unk_het = Column(Integer)
    query_unk_homalt = Column(Integer)
    
    # Relationship
    experiment = relationship("UserExperiment", back_populates="benchmark_results")
    
    def __repr__(self):
        return f"<UserBenchmarkResult(exp_id={self.experiment_id}, type={self.variant_type})>"