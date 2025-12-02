# ============================================================================
# dynamic_options.R
# ============================================================================
# Helper functions to fetch dropdown options dynamically from database.
# Includes hierarchical checkbox generation for nested tech/caller selection.

# ============================================================================
# FETCH FUNCTIONS
# ============================================================================

get_technology_choices <- function() {
  # Get distinct technologies from database
 tryCatch({
    techs <- db$get_distinct_technologies()
    if (length(techs) == 0) {
      return(c("ILLUMINA", "PACBIO", "ONT", "MGI"))
    }
    
    display_names <- sapply(techs, function(t) {
      switch(t,
        "ILLUMINA" = "Illumina",
        "PACBIO" = "PacBio", 
        "ONT" = "ONT",
        "MGI" = "MGI",
        "10X" = "10X",
        t
      )
    })
    setNames(techs, display_names)
  }, error = function(e) {
    warning(paste("Error fetching technologies:", e$message))
    c("ILLUMINA" = "Illumina", "PACBIO" = "PacBio", "ONT" = "ONT", "MGI" = "MGI")
  })
}

get_caller_choices <- function() {
  # Get distinct callers from database
  tryCatch({
    callers <- db$get_distinct_callers()
    if (length(callers) == 0) {
      return(c("DEEPVARIANT", "GATK", "CLAIR3"))
    }
    
    display_names <- sapply(callers, function(c) {
      switch(c,
        "DEEPVARIANT" = "DeepVariant",
        "GATK" = "GATK",
        "CLAIR3" = "Clair3",
        "DRAGEN" = "DRAGEN",
        "GATK3" = "GATK3",
        "GATK4" = "GATK4",
        "LONGRANGER" = "LongRanger",
        "MEGABOLT" = "Megabolt",
        "NANOCALLER" = "NanoCaller",
        "PARABRICK" = "Parabrick",
        "PEPPER" = "Pepper",
        c
      )
    })
    setNames(callers, display_names)
  }, error = function(e) {
    warning(paste("Error fetching callers:", e$message))
    c("DEEPVARIANT" = "DeepVariant", "GATK" = "GATK", "CLAIR3" = "Clair3")
  })
}

get_platform_choices <- function(technology) {
  if (is.null(technology) || technology == "") return(character(0))
  
  tryCatch({
    platforms <- db$get_platforms_for_technology(technology)
    if (length(platforms) == 0) return(character(0))
    setNames(platforms, platforms)
  }, error = function(e) {
    warning(paste("Error fetching platforms:", e$message))
    character(0)
  })
}

get_version_choices <- function(caller) {
  if (is.null(caller) || caller == "") return(character(0))
  
  tryCatch({
    versions <- db$get_versions_for_caller(caller)
    if (length(versions) == 0) return(character(0))
    setNames(versions, versions)
  }, error = function(e) {
    warning(paste("Error fetching versions:", e$message))
    character(0)
  })
}

get_chemistry_choices <- function(technology) {
  if (is.null(technology) || technology == "") return(character(0))
  
  tryCatch({
    chemistries <- db$get_chemistries_for_technology(technology)
    if (length(chemistries) == 0) return(character(0))
    setNames(chemistries, chemistries)
  }, error = function(e) {
    warning(paste("Error fetching chemistries:", e$message))
    character(0)
  })
}

# ============================================================================
# HIERARCHICAL DATA STRUCTURES
# ============================================================================

get_technology_hierarchy <- function() {
  # Returns list: technology -> list of platforms
  techs <- db$get_distinct_technologies()
  
  hierarchy <- list()
  for (tech in techs) {
    platforms <- tryCatch({
      db$get_platforms_for_technology(tech)
    }, error = function(e) character(0))
    
    display_name <- switch(tech,
      "ILLUMINA" = "Illumina",
      "PACBIO" = "PacBio", 
      "ONT" = "ONT",
      "MGI" = "MGI",
      "10X" = "10X",
      tech
    )
    
    hierarchy[[tech]] <- list(
      display_name = display_name,
      platforms = if(length(platforms) > 0) platforms else character(0)
    )
  }
  hierarchy
}

get_caller_hierarchy <- function() {
  # Returns list: caller -> list of versions
  callers <- db$get_distinct_callers()
  
  hierarchy <- list()
  for (caller in callers) {
    versions <- tryCatch({
      db$get_versions_for_caller(caller)
    }, error = function(e) character(0))
    
    display_name <- switch(caller,
      "DEEPVARIANT" = "DeepVariant",
      "GATK" = "GATK",
      "CLAIR3" = "Clair3",
      "DRAGEN" = "DRAGEN",
      "GATK3" = "GATK3",
      "GATK4" = "GATK4",
      "LONGRANGER" = "LongRanger",
      "MEGABOLT" = "Megabolt",
      "NANOCALLER" = "NanoCaller",
      "PARABRICK" = "Parabrick",
      "PEPPER" = "Pepper",
      caller
    )
    
    hierarchy[[caller]] <- list(
      display_name = display_name,
      versions = if(length(versions) > 0) versions else character(0)
    )
  }
  hierarchy
}

# ============================================================================
# HIERARCHICAL CHECKBOX UI GENERATOR
# ============================================================================

create_hierarchical_tech_ui <- function(input_id_prefix = "tech") {
  # Creates compact nested checkbox UI for technologies and platforms
  hierarchy <- get_technology_hierarchy()
  
  if (length(hierarchy) == 0) {
    return(p("No technologies found", style = "color: #999; font-size: 12px;"))
  }
  
  tech_divs <- lapply(names(hierarchy), function(tech) {
    tech_info <- hierarchy[[tech]]
    tech_checkbox_id <- paste0(input_id_prefix, "_", tech)
    platforms <- tech_info$platforms
    
    # Parent technology checkbox
    parent_checkbox <- div(
      class = "tech-parent-item",
      tags$label(
        style = "display: flex; align-items: center; cursor: pointer;",
        tags$input(
          type = "checkbox",
          class = "tech-parent-checkbox",
          `data-tech` = tech,
          id = tech_checkbox_id
        ),
        if (length(platforms) > 0) {
          tags$span(
            class = "tech-expand-arrow",
            `data-tech` = tech,
            style = "cursor: pointer;",
            "▶"
          )
        },
        tech_info$display_name
      )
    )
    
    # Child platform checkboxes
    child_div <- if (length(platforms) > 0) {
      div(
        id = paste0(input_id_prefix, "_platforms_", tech),
        class = "tech-children-container",
        `data-tech` = tech,
        style = "display: none;",
        lapply(platforms, function(platform) {
          div(
            tags$label(
              style = "display: flex; align-items: center; cursor: pointer;",
              tags$input(
                type = "checkbox",
                class = "tech-child-checkbox",
                `data-tech` = tech,
                `data-platform` = platform
              ),
              platform
            )
          )
        })
      )
    } else NULL
    
    div(parent_checkbox, child_div)
  })
  
  div(
    id = paste0(input_id_prefix, "_hierarchy_container"),
    class = "hierarchical-checkbox-container",
    tech_divs,
    tags$input(
      type = "hidden",
      id = paste0(input_id_prefix, "_selection"),
      class = "hierarchy-selection-input",
      value = "{}"
    )
  )
}

create_hierarchical_caller_ui <- function(input_id_prefix = "caller") {
  # Creates nested checkbox UI for callers and versions
  hierarchy <- get_caller_hierarchy()
  
  if (length(hierarchy) == 0) {
    return(p("No callers found", style = "color: #999;"))
  }
  
  caller_divs <- lapply(names(hierarchy), function(caller) {
    caller_info <- hierarchy[[caller]]
    caller_checkbox_id <- paste0(input_id_prefix, "_", caller)
    versions <- caller_info$versions
    
    # Parent caller checkbox
    parent_checkbox <- div(
      class = "caller-parent-item",
      style = "margin-bottom: 4px;",
      tags$label(
        style = "display: flex; align-items: center; cursor: pointer; font-weight: 500;",
        tags$input(
          type = "checkbox",
          class = "caller-parent-checkbox",
          `data-caller` = caller,
          id = caller_checkbox_id,
          style = "margin-right: 8px;"
        ),
        # Expand arrow (only if has versions)
        if (length(versions) > 0) {
          tags$span(
            class = "caller-expand-arrow",
            `data-caller` = caller,
            style = "margin-right: 6px; cursor: pointer; color: #6c757d; font-size: 10px;",
            "▶"
          )
        },
        caller_info$display_name
      )
    )
    
    # Child version checkboxes (collapsible)
    child_div <- if (length(versions) > 0) {
      div(
        id = paste0(input_id_prefix, "_versions_", caller),
        class = "caller-children-container",
        `data-caller` = caller,
        style = "display: none; margin-left: 28px; margin-top: 4px; margin-bottom: 8px; padding-left: 10px; border-left: 2px solid #e0e0e0;",
        lapply(versions, function(version) {
          div(
            style = "margin-bottom: 2px;",
            tags$label(
              style = "display: flex; align-items: center; cursor: pointer; font-weight: 400; font-size: 13px; color: #555;",
              tags$input(
                type = "checkbox",
                class = "caller-child-checkbox",
                `data-caller` = caller,
                `data-version` = version,
                style = "margin-right: 8px;"
              ),
              version
            )
          )
        })
      )
    } else NULL
    
    div(parent_checkbox, child_div)
  })
  
  # Wrap in container with hidden input to store selection
  div(
    id = paste0(input_id_prefix, "_hierarchy_container"),
    class = "hierarchical-checkbox-container",
    caller_divs,
    # Hidden input to store JSON selection state (read by Shiny)
    tags$input(
      type = "hidden",
      id = paste0(input_id_prefix, "_selection"),
      class = "hierarchy-selection-input",
      value = "{}"
    )
  )
}

# ============================================================================
# JAVASCRIPT FOR HIERARCHICAL CHECKBOXES
# ============================================================================

HIERARCHICAL_CHECKBOX_JS <- '
$(document).ready(function() {
  
  // Toggle expand/collapse for technology platforms
  $(document).on("click", ".tech-expand-arrow", function(e) {
    e.preventDefault();
    e.stopPropagation();
    var tech = $(this).data("tech");
    var container = $(".tech-children-container[data-tech=\'" + tech + "\']");
    
    if (container.is(":visible")) {
      container.slideUp(150);
      $(this).text("▶");
    } else {
      container.slideDown(150);
      $(this).text("▼");
    }
  });
  
  // Toggle expand/collapse for caller versions  
  $(document).on("click", ".caller-expand-arrow", function(e) {
    e.preventDefault();
    e.stopPropagation();
    var caller = $(this).data("caller");
    var container = $(".caller-children-container[data-caller=\'" + caller + "\']");
    
    if (container.is(":visible")) {
      container.slideUp(150);
      $(this).text("▶");
    } else {
      container.slideDown(150);
      $(this).text("▼");
    }
  });
  
  // Technology parent checkbox change
  $(document).on("change", ".tech-parent-checkbox", function() {
    var tech = $(this).data("tech");
    var isChecked = $(this).is(":checked");
    
    // If parent checked, uncheck all children (means "all platforms")
    // If parent unchecked, uncheck all children too
    $(".tech-child-checkbox[data-tech=\'" + tech + "\']").prop("checked", false);
    
    updateTechSelection();
  });
  
  // Technology child checkbox change
  $(document).on("change", ".tech-child-checkbox", function() {
    var tech = $(this).data("tech");
    var anyChildChecked = $(".tech-child-checkbox[data-tech=\'" + tech + "\']:checked").length > 0;
    
    // If any child is checked, uncheck the parent (specific platforms selected)
    // Parent checked = all platforms, children checked = specific platforms
    if (anyChildChecked) {
      $(".tech-parent-checkbox[data-tech=\'" + tech + "\']").prop("checked", false);
    }
    
    updateTechSelection();
  });
  
  // Caller parent checkbox change
  $(document).on("change", ".caller-parent-checkbox", function() {
    var caller = $(this).data("caller");
    
    // Uncheck all children when parent changes
    $(".caller-child-checkbox[data-caller=\'" + caller + "\']").prop("checked", false);
    
    updateCallerSelection();
  });
  
  // Caller child checkbox change
  $(document).on("change", ".caller-child-checkbox", function() {
    var caller = $(this).data("caller");
    var anyChildChecked = $(".caller-child-checkbox[data-caller=\'" + caller + "\']:checked").length > 0;
    
    if (anyChildChecked) {
      $(".caller-parent-checkbox[data-caller=\'" + caller + "\']").prop("checked", false);
    }
    
    updateCallerSelection();
  });
  
  // Update technology selection and send to Shiny
  function updateTechSelection() {
    var selection = {};
    
    // Get parent selections (all platforms for that tech)
    $(".tech-parent-checkbox:checked").each(function() {
      var tech = $(this).data("tech");
      selection[tech] = null; // null means all platforms
    });
    
    // Get child selections (specific platforms)
    $(".tech-child-checkbox:checked").each(function() {
      var tech = $(this).data("tech");
      var platform = $(this).data("platform");
      
      if (!selection[tech]) {
        selection[tech] = [];
      }
      if (selection[tech] !== null) {
        selection[tech].push(platform);
      }
    });
    
    // Send to Shiny
    Shiny.setInputValue("tech_hierarchy_selection", selection, {priority: "event"});
  }
  
  // Update caller selection and send to Shiny
  function updateCallerSelection() {
    var selection = {};
    
    // Get parent selections (all versions for that caller)
    $(".caller-parent-checkbox:checked").each(function() {
      var caller = $(this).data("caller");
      selection[caller] = null; // null means all versions
    });
    
    // Get child selections (specific versions)
    $(".caller-child-checkbox:checked").each(function() {
      var caller = $(this).data("caller");
      var version = $(this).data("version");
      
      if (!selection[caller]) {
        selection[caller] = [];
      }
      if (selection[caller] !== null) {
        selection[caller].push(version);
      }
    });
    
    // Send to Shiny
    Shiny.setInputValue("caller_hierarchy_selection", selection, {priority: "event"});
  }
});
'

# CSS for hierarchical checkboxes
HIERARCHICAL_CHECKBOX_CSS <- '
.hierarchical-checkbox-container {
  max-height: 200px;
  overflow-y: auto;
  padding: 6px 8px;
  border: 1px solid #e0e0e0;
  border-radius: 4px;
  background: #fff;
  font-size: 13px;
}

.hierarchical-checkbox-container::-webkit-scrollbar {
  width: 6px;
}

.hierarchical-checkbox-container::-webkit-scrollbar-thumb {
  background: #ccc;
  border-radius: 3px;
}

.tech-parent-item, .caller-parent-item {
  padding: 3px 0;
  line-height: 1.3;
}

.tech-parent-item label, .caller-parent-item label {
  font-size: 13px !important;
  font-weight: 500 !important;
  margin-bottom: 0 !important;
}

.tech-parent-item input[type="checkbox"],
.caller-parent-item input[type="checkbox"] {
  margin-right: 6px !important;
  transform: scale(0.9);
}

.tech-children-container, .caller-children-container {
  margin-left: 20px !important;
  margin-top: 2px !important;
  margin-bottom: 4px !important;
  padding-left: 8px !important;
  border-left: 2px solid #e8e8e8 !important;
}

.tech-children-container label, .caller-children-container label {
  font-size: 12px !important;
  font-weight: 400 !important;
  color: #555 !important;
  margin-bottom: 0 !important;
}

.tech-children-container input[type="checkbox"],
.caller-children-container input[type="checkbox"] {
  margin-right: 5px !important;
  transform: scale(0.85);
}

.tech-children-container > div, .caller-children-container > div {
  padding: 1px 0 !important;
  margin-bottom: 0 !important;
}

.tech-expand-arrow, .caller-expand-arrow {
  user-select: none;
  width: 10px;
  text-align: center;
  font-size: 9px !important;
  color: #888 !important;
  margin-right: 4px !important;
}

.tech-expand-arrow:hover, .caller-expand-arrow:hover {
  color: #5084d1 !important;
}
'