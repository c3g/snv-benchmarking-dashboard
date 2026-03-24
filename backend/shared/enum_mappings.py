# ============================================================================
# enum_mappings.py
# ============================================================================
"""
Single source of truth for enum mappings and validation lists.

All mappings are auto-derived from models.py enums.
Add new enum values ONLY in models.py — this file auto-updates.

SNV and SV callers are kept in separate mapping dicts (VALID_SNV_CALLERS vs
VALID_SV_CALLERS)

To add aliases (e.g., '10x genomics' -> TENX), add to ALIASES dict below.

"""

from models import (
    SeqTechName, SeqTechTarget, SeqTechPlatformType,
    SNVCallerName, SVCallerName, CallerType,
    TruthSetName, TruthSetReference, TruthSetSample,
    VariantOrigin, SNVType,
    SVType, SVSizeRange,
    BenchmarkToolName
)

# ============================================================================
# ALIASES
# ============================================================================

ALIASES = {
    'technology': {
        '10x genomics': SeqTechName.TENX,
    },
    'benchmark_tool_name': {
        'hap.py': BenchmarkToolName.HAPPY,
        'happy': BenchmarkToolName.HAPPY,
    },
    'snv_type': {
        'snp+indel': SNVType.SNPINDEL,
        'snpindel':  SNVType.SNPINDEL,
    },
}

# ============================================================================
# HELPERS
# ============================================================================

def _build_mapping(enum_class, aliases=None):
    """Build lowercase string -> enum mapping. Aliases override auto-generated entries."""
    mapping = {}
    for member in enum_class:
        mapping[member.value.lower()] = member
        mapping[member.name.lower()] = member
    if aliases:
        mapping.update(aliases)
    return mapping


def _build_valid_list(enum_class):
    """Build list of valid uppercase value strings for validation."""
    return [member.value.upper() for member in enum_class]

# ============================================================================
# ENUM MAPPINGS — used by direct_db_population.py
# ============================================================================

ENUM_MAPPINGS = {
    'technology':          _build_mapping(SeqTechName, ALIASES.get('technology')),
    'target':              _build_mapping(SeqTechTarget),
    'platform_type':       _build_mapping(SeqTechPlatformType),
    'snv_caller_name':     _build_mapping(SNVCallerName),
    'sv_caller_name':      _build_mapping(SVCallerName),
    'caller_type':         _build_mapping(CallerType),
    'truth_set_name':      _build_mapping(TruthSetName),
    'truth_set_reference': _build_mapping(TruthSetReference),
    'truth_set_sample':    _build_mapping(TruthSetSample),
    'variant_origin':      _build_mapping(VariantOrigin),
    'snv_type':            _build_mapping(SNVType, ALIASES.get('snv_type')),
    'sv_type':             _build_mapping(SVType),
    'sv_size_range':       _build_mapping(SVSizeRange),
    'benchmark_tool_name': _build_mapping(BenchmarkToolName, ALIASES.get('benchmark_tool_name')),
}

# ============================================================================
# VALID LISTS — used by upload_handler.py for validation
# ============================================================================

VALID_TECHNOLOGIES     = _build_valid_list(SeqTechName)
VALID_SNV_CALLERS      = _build_valid_list(SNVCallerName)
VALID_SV_CALLERS       = _build_valid_list(SVCallerName)
VALID_TRUTH_SETS       = _build_valid_list(TruthSetName)
VALID_TARGETS          = _build_valid_list(SeqTechTarget)
VALID_PLATFORM_TYPES   = _build_valid_list(SeqTechPlatformType)
VALID_CALLER_TYPES     = _build_valid_list(CallerType)
VALID_REFERENCES       = _build_valid_list(TruthSetReference)
VALID_SAMPLES          = _build_valid_list(TruthSetSample)
VALID_SNV_TYPES        = _build_valid_list(SNVType)
VALID_VARIANT_ORIGINS  = _build_valid_list(VariantOrigin)
VALID_VARIANT_ORIGINS  = _build_valid_list(VariantOrigin)
VALID_SV_TYPES         = _build_valid_list(SVType)
VALID_SV_SIZE_RANGES   = _build_valid_list(SVSizeRange)
VALID_BENCHMARK_TOOLS  = _build_valid_list(BenchmarkToolName)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

def map_enum(field_name, value):
    """
    Map string value to enum (case-insensitive).
    field_name must be a key in ENUM_MAPPINGS (e.g. 'technology', 'snv_caller_name').
    Returns None if not found.
    """
    if not value or value == '' or value is None:
        return None
    value_clean = str(value).strip().lower()
    if field_name not in ENUM_MAPPINGS:
        return None
    return ENUM_MAPPINGS[field_name].get(value_clean)


def map_boolean(value):
    """Convert string/bool to boolean."""
    if isinstance(value, bool):
        return value
    if value is None:
        return False
    return str(value).strip().lower() == 'true'


def is_valid_enum_value(field_name, value):
    """Check if value is valid for the given enum field."""
    return map_enum(field_name, value) is not None