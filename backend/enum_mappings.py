# ============================================================================
# enum_mappings.py
# ============================================================================
"""
Single source of truth for enum mappings and validation lists.

All enum-related mappings are auto-derived from models.py enums.
Add new enum values ONLY in models.py - this file auto-updates.

To add aliases (e.g., '10x genomics' -> TENX), add to ALIASES dict below.
"""

from models import (
    SeqTechName, SeqTechTarget, SeqTechPlatformType,
    CallerName, CallerType,
    TruthSetName, TruthSetReference, TruthSetSample,
    VariantOrigin, VariantSize, VariantType,
    BenchmarkToolName
)

# ============================================================================
# ALIASES - Manual mappings for alternative names
# ============================================================================

ALIASES = {
    'technology': {
        '10x genomics': SeqTechName.TENX,
    },
    'benchmark_tool_name': {
        'hap.py': BenchmarkToolName.HAPPY,
        'happy': BenchmarkToolName.HAPPY,
    },
    'variant_type': {
        'snp+indel': VariantType.SNPINDEL,
        'snpindel': VariantType.SNPINDEL,
    }
}

# ============================================================================
# AUTO-GENERATED MAPPINGS
# ============================================================================

def _build_mapping(enum_class, aliases=None):
    """
    Build lowercase string -> enum mapping from enum class.
    
    Args:
        enum_class: Enum class to build mapping from
        aliases: Optional dict of additional string -> enum mappings
    
    Returns:
        dict: Lowercase string keys mapping to enum values
    """
    mapping = {}
    for member in enum_class:
        mapping[member.value.lower()] = member
        mapping[member.name.lower()] = member
    
    if aliases:
        mapping.update(aliases)
    
    return mapping

def _build_valid_list(enum_class):
    """
    Build list of valid uppercase values for validation.
    
    Args:
        enum_class: Enum class to extract values from
    
    Returns:
        list: Uppercase string values
    """
    return [member.value.upper() for member in enum_class]

# ============================================================================
# ENUM MAPPINGS - Used by direct_db_population.py
# ============================================================================

ENUM_MAPPINGS = {
    'technology': _build_mapping(SeqTechName, ALIASES.get('technology')),
    'target': _build_mapping(SeqTechTarget),
    'platform_type': _build_mapping(SeqTechPlatformType),
    'caller_name': _build_mapping(CallerName),
    'caller_type': _build_mapping(CallerType),
    'truth_set_name': _build_mapping(TruthSetName),
    'truth_set_reference': _build_mapping(TruthSetReference),
    'truth_set_sample': _build_mapping(TruthSetSample),
    'variant_origin': _build_mapping(VariantOrigin),
    'variant_size': _build_mapping(VariantSize),
    'variant_type': _build_mapping(VariantType, ALIASES.get('variant_type')),
    'benchmark_tool_name': _build_mapping(BenchmarkToolName, ALIASES.get('benchmark_tool_name')),
}

# ============================================================================
# VALID LISTS - Used by upload_handler.py for validation
# ============================================================================

VALID_TECHNOLOGIES = _build_valid_list(SeqTechName)
VALID_CALLERS = _build_valid_list(CallerName)
VALID_TRUTH_SETS = _build_valid_list(TruthSetName)
VALID_TARGETS = _build_valid_list(SeqTechTarget)
VALID_PLATFORM_TYPES = _build_valid_list(SeqTechPlatformType)
VALID_CALLER_TYPES = _build_valid_list(CallerType)
VALID_REFERENCES = _build_valid_list(TruthSetReference)
VALID_SAMPLES = _build_valid_list(TruthSetSample)
VALID_VARIANT_TYPES = _build_valid_list(VariantType)
VALID_VARIANT_SIZES = _build_valid_list(VariantSize)
VALID_VARIANT_ORIGINS = _build_valid_list(VariantOrigin)
VALID_BENCHMARK_TOOLS = _build_valid_list(BenchmarkToolName)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

def map_enum(field_name, value):
    """
    Map string value to enum type with case-insensitive matching.
    
    Args:
        field_name: Key in ENUM_MAPPINGS (e.g., 'technology', 'caller_name')
        value: String value to map
    
    Returns:
        Enum value or None if not found
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
    """
    Check if value is valid for given enum field.
    
    Args:
        field_name: Key in ENUM_MAPPINGS
        value: Value to check
    
    Returns:
        bool: True if valid, False otherwise
    """
    return map_enum(field_name, value) is not None