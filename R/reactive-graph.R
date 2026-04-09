#' Internal helper `.rtuiNextReactiveId`.
#'
#' Generates a runtime-unique reactive id using a prefix.
#'
#' @param prefix Prefix used to build a new reactive identifier.
#'
#' @return A runtime-unique reactive id.
#'
#' @keywords internal
#' @noRd
.rtuiNextReactiveId <- function(prefix) {
  runtime <- .rtuiCurrentRuntime()
  runtime$reactiveIndex <- runtime$reactiveIndex + 1L
  paste0(prefix, "_", runtime$reactiveIndex)
}

#' Internal helper `.rtuiReactiveStoreGet`.
#'
#' Retrieves a reactive-state record from `runtime$reactiveState`.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#'
#' @return Stored state list for `reactiveId`, or `NULL` if missing.
#'
#' @keywords internal
#' @noRd
.rtuiReactiveStoreGet <- function(runtime, reactiveId) {
  if (exists(reactiveId, envir = runtime$reactiveState, inherits = FALSE)) {
    return(get(reactiveId, envir = runtime$reactiveState, inherits = FALSE))
  }
  NULL
}

#' Internal helper `.rtuiReactiveStoreSet`.
#'
#' Stores a reactive-state record in `runtime$reactiveState`.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#' @param value Value to evaluate, resolve, or store.
#' @param hasValue Whether the reactive state currently has a computed value.
#' @param dirty Whether the reactive state should be recomputed.
#'
#' @return The state entry written to `runtime$reactiveState`.
#'
#' @keywords internal
#' @noRd
.rtuiReactiveStoreSet <- function(runtime, reactiveId, value, hasValue = TRUE, dirty = FALSE) {
  assign(
    reactiveId,
    list(value = value, hasValue = hasValue, dirty = isTRUE(dirty)),
    envir = runtime$reactiveState
  )
}

#' Internal helper `.rtuiReactiveStoreEnsure`.
#'
#' Ensures a reactive-state record exists and has the expected fields.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#' @param value Value to evaluate, resolve, or store.
#' @param hasValue Whether the reactive state currently has a computed value.
#' @param dirty Whether the reactive state should be recomputed.
#'
#' @return A reactive state entry list with `value`, `hasValue`, and `dirty`.
#'
#' @keywords internal
#' @noRd
.rtuiReactiveStoreEnsure <- function(
    runtime,
    reactiveId,
    value = NULL,
    hasValue = FALSE,
    dirty = !isTRUE(hasValue)
) {
  current <- .rtuiReactiveStoreGet(runtime, reactiveId)
  if (is.null(current)) {
    .rtuiReactiveStoreSet(
      runtime,
      reactiveId,
      value = value,
      hasValue = hasValue,
      dirty = dirty
    )
    return(.rtuiReactiveStoreGet(runtime, reactiveId))
  }

  if (is.null(current$dirty)) {
    current$dirty <- !isTRUE(current$hasValue)
    assign(reactiveId, current, envir = runtime$reactiveState)
  }
  current
}

#' Internal helper `.rtuiReactiveStoreMarkDirty`.
#'
#' Marks a reactive-state record as dirty or clean.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#' @param dirty Whether the reactive state should be recomputed.
#'
#' @return Updated reactive state entry (side effect on `runtime$reactiveState`).
#'
#' @keywords internal
#' @noRd
.rtuiReactiveStoreMarkDirty <- function(runtime, reactiveId, dirty = TRUE) {
  current <- .rtuiReactiveStoreEnsure(runtime, reactiveId)
  current$dirty <- isTRUE(dirty)
  assign(reactiveId, current, envir = runtime$reactiveState)
}

#' Internal helper `.rtuiReactiveCacheKey`.
#'
#' Builds a cache key for a reactive node type/id pair.
#'
#' @param nodeType Internal node type label.
#' @param reactiveId Internal identifier of a reactive node.
#'
#' @return A single cache key string.
#'
#' @keywords internal
#' @noRd
.rtuiReactiveCacheKey <- function(nodeType, reactiveId) {
  paste0(nodeType, ":", reactiveId)
}

#' Internal helper `.rtuiDropReactiveCache`.
#'
#' Drops cached values for a reactive id across reactive node types.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiDropReactiveCache <- function(runtime, reactiveId) {
  cachePrefixes <- c("reactive", "reactiveVal", "reactiveEvent")
  for (prefix in cachePrefixes) {
    cacheKey <- .rtuiReactiveCacheKey(prefix, reactiveId)
    if (exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
      rm(list = cacheKey, envir = runtime$currentReactiveCache)
    }
  }
}

#' Internal helper `.rtuiGraphMapGet`.
#'
#' Reads graph map entries and normalizes them as unique character vectors.
#'
#' @param mapEnv Environment used as a string-keyed graph map.
#' @param key Map key to read or write.
#'
#' @return Unique character vector for `key`, or `character()`.
#'
#' @keywords internal
#' @noRd
.rtuiGraphMapGet <- function(mapEnv, key) {
  if (!exists(key, envir = mapEnv, inherits = FALSE)) {
    return(character())
  }

  value <- get(key, envir = mapEnv, inherits = FALSE)
  if (is.null(value) || length(value) == 0L) {
    return(character())
  }
  unique(as.character(value))
}

#' Internal helper `.rtuiGraphMapSet`.
#'
#' Writes graph map entries, removing keys when values are empty.
#'
#' @param mapEnv Environment used as a string-keyed graph map.
#' @param key Map key to read or write.
#' @param values Values to store for `key` in the map.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiGraphMapSet <- function(mapEnv, key, values) {
  normalized <- unique(as.character(values))
  if (length(normalized) == 0L) {
    if (exists(key, envir = mapEnv, inherits = FALSE)) {
      rm(list = key, envir = mapEnv)
    }
    return(invisible(NULL))
  }

  assign(key, normalized, envir = mapEnv)
  invisible(NULL)
}

#' Internal helper `.rtuiInputNodeId`.
#'
#' Builds the graph node id used for an input.
#'
#' @param inputId Input identifier from the UI definition.
#'
#' @return A graph node id of the form `"input:<id>"`.
#'
#' @keywords internal
#' @noRd
.rtuiInputNodeId <- function(inputId) {
  paste0("input:", inputId)
}

#' Internal helper `.rtuiGraphEnsureRuntime`.
#'
#' Initializes dependency-graph runtime fields when missing.
#'
#' @param runtime Runtime environment created for the current app instance.
#'
#' @return Invisibly returns `NULL` after ensuring required runtime fields.
#'
#' @keywords internal
#' @noRd
.rtuiGraphEnsureRuntime <- function(runtime) {
  if (is.null(runtime$graphDependencies) || !is.environment(runtime$graphDependencies)) {
    runtime$graphDependencies <- new.env(parent = emptyenv())
  }
  if (is.null(runtime$graphDependents) || !is.environment(runtime$graphDependents)) {
    runtime$graphDependents <- new.env(parent = emptyenv())
  }
  if (is.null(runtime$graphNodeTypes) || !is.environment(runtime$graphNodeTypes)) {
    runtime$graphNodeTypes <- new.env(parent = emptyenv())
  }
  if (is.null(runtime$currentEvalDeps) || !is.environment(runtime$currentEvalDeps)) {
    runtime$currentEvalDeps <- new.env(parent = emptyenv())
  }
  if (is.null(runtime$graphEvalStack)) {
    runtime$graphEvalStack <- character()
  }
  if (is.null(runtime$currentRunId) || length(runtime$currentRunId) == 0L) {
    runtime$currentRunId <- 0L
  }
  if (is.null(runtime$currentInputState) || !is.list(runtime$currentInputState)) {
    runtime$currentInputState <- list()
  }
  if (is.null(runtime$currentCaptureDepth) || length(runtime$currentCaptureDepth) == 0L) {
    runtime$currentCaptureDepth <- 0L
  }
}

#' Internal helper `.rtuiGraphSetNodeType`.
#'
#' Associates a node id with its graph node type.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param nodeId Graph node identifier.
#' @param nodeType Internal node type label.
#'
#' @return Assigned node type value.
#'
#' @keywords internal
#' @noRd
.rtuiGraphSetNodeType <- function(runtime, nodeId, nodeType) {
  assign(nodeId, nodeType, envir = runtime$graphNodeTypes)
}

#' Internal helper `.rtuiGraphNodeType`.
#'
#' Returns a node type, defaulting to `"reactive"` when unset.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param nodeId Graph node identifier.
#'
#' @return Node type string for `nodeId`.
#'
#' @keywords internal
#' @noRd
.rtuiGraphNodeType <- function(runtime, nodeId) {
  if (!exists(nodeId, envir = runtime$graphNodeTypes, inherits = FALSE)) {
    return("reactive")
  }
  get(nodeId, envir = runtime$graphNodeTypes, inherits = FALSE)
}

#' Internal helper `.rtuiGraphEnsureReactiveNode`.
#'
#' Ensures graph metadata and reactive-state storage exist for a node.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#' @param nodeType Internal node type label.
#' @param defaultValue Initial value used when creating a missing state entry.
#' @param hasValue Whether the reactive state currently has a computed value.
#'
#' @return Invisibly returns the ensured state entry.
#'
#' @keywords internal
#' @noRd
.rtuiGraphEnsureReactiveNode <- function(
    runtime,
    reactiveId,
    nodeType,
    defaultValue = NULL,
    hasValue = FALSE
) {
  .rtuiGraphEnsureRuntime(runtime)
  .rtuiGraphSetNodeType(runtime, reactiveId, nodeType)
  .rtuiReactiveStoreEnsure(
    runtime,
    reactiveId,
    value = defaultValue,
    hasValue = hasValue,
    dirty = !isTRUE(hasValue)
  )
}

#' Internal helper `.rtuiGraphUpdateDependencies`.
#'
#' Updates dependency and dependent edges for a graph node.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param nodeId Graph node identifier.
#' @param dependencyIds Dependency node ids currently read by `nodeId`.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiGraphUpdateDependencies <- function(runtime, nodeId, dependencyIds) {
  nextDeps <- unique(as.character(dependencyIds))
  previousDeps <- .rtuiGraphMapGet(runtime$graphDependencies, nodeId)

  removedDeps <- setdiff(previousDeps, nextDeps)
  addedDeps <- setdiff(nextDeps, previousDeps)

  .rtuiGraphMapSet(runtime$graphDependencies, nodeId, nextDeps)

  for (dependencyId in removedDeps) {
    dependents <- .rtuiGraphMapGet(runtime$graphDependents, dependencyId)
    .rtuiGraphMapSet(
      runtime$graphDependents,
      dependencyId,
      setdiff(dependents, nodeId)
    )
  }

  for (dependencyId in addedDeps) {
    dependents <- .rtuiGraphMapGet(runtime$graphDependents, dependencyId)
    .rtuiGraphMapSet(
      runtime$graphDependents,
      dependencyId,
      c(dependents, nodeId)
    )
  }
}

#' Internal helper `.rtuiGraphBeginEvaluation`.
#'
#' Starts evaluation tracking for a node and detects cyclic dependencies.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiGraphBeginEvaluation <- function(runtime, reactiveId) {
  .rtuiGraphEnsureRuntime(runtime)
  stack <- runtime$graphEvalStack
  if (reactiveId %in% stack) {
    stop("Cyclic reactive dependency detected for `", reactiveId, "`.")
  }

  runtime$graphEvalStack <- c(stack, reactiveId)
  .rtuiGraphMapSet(runtime$currentEvalDeps, reactiveId, character())
}

#' Internal helper `.rtuiGraphRegisterRead`.
#'
#' Registers that the currently evaluated node read a dependency.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param dependencyId Single dependency node id being registered.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiGraphRegisterRead <- function(runtime, dependencyId) {
  if (.rtuiInIsolate(runtime) || .rtuiCaptureSuspended(runtime)) {
    return(invisible(NULL))
  }

  .rtuiGraphEnsureRuntime(runtime)
  stack <- runtime$graphEvalStack
  if (length(stack) == 0L) {
    return(invisible(NULL))
  }

  currentNodeId <- stack[[length(stack)]]
  dependencies <- .rtuiGraphMapGet(runtime$currentEvalDeps, currentNodeId)
  if (!(dependencyId %in% dependencies)) {
    .rtuiGraphMapSet(
      runtime$currentEvalDeps,
      currentNodeId,
      c(dependencies, dependencyId)
    )
  }

  invisible(NULL)
}

#' Internal helper `.rtuiGraphEndEvaluation`.
#'
#' Ends evaluation tracking and persists dependencies when evaluation succeeded.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#' @param success Whether the evaluation completed successfully.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiGraphEndEvaluation <- function(runtime, reactiveId, success) {
  .rtuiGraphEnsureRuntime(runtime)
  stack <- runtime$graphEvalStack

  if (length(stack) > 0L) {
    if (identical(stack[[length(stack)]], reactiveId)) {
      runtime$graphEvalStack <- stack[-length(stack)]
    } else {
      runtime$graphEvalStack <- stack[stack != reactiveId]
    }
  }

  if (!isTRUE(success)) {
    if (exists(reactiveId, envir = runtime$currentEvalDeps, inherits = FALSE)) {
      rm(list = reactiveId, envir = runtime$currentEvalDeps)
    }
    return(invisible(NULL))
  }

  dependencies <- .rtuiGraphMapGet(runtime$currentEvalDeps, reactiveId)
  if (exists(reactiveId, envir = runtime$currentEvalDeps, inherits = FALSE)) {
    rm(list = reactiveId, envir = runtime$currentEvalDeps)
  }
  .rtuiGraphUpdateDependencies(runtime, reactiveId, dependencies)
  invisible(NULL)
}

#' Internal helper `.rtuiGraphInvalidateDependents`.
#'
#' Marks downstream dependents dirty and clears their caches.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param sourceId Source node id whose dependents must be invalidated.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiGraphInvalidateDependents <- function(runtime, sourceId) {
  .rtuiGraphEnsureRuntime(runtime)
  queue <- c(sourceId)
  seen <- character()

  while (length(queue) > 0L) {
    currentId <- queue[[1L]]
    queue <- queue[-1L]
    if (currentId %in% seen) {
      next
    }
    seen <- c(seen, currentId)

    dependents <- .rtuiGraphMapGet(runtime$graphDependents, currentId)
    if (length(dependents) == 0L) {
      next
    }

    for (dependentId in dependents) {
      .rtuiReactiveStoreMarkDirty(runtime, dependentId, dirty = TRUE)
      .rtuiDropReactiveCache(runtime, dependentId)

      if (!identical(.rtuiGraphNodeType(runtime, sourceId), "reactiveVal")) {
        next
      }

      warningNodes <- character()
      if (
        length(runtime$graphEvalStack) > 0L &&
          dependentId %in% runtime$graphEvalStack &&
          identical(.rtuiGraphNodeType(runtime, dependentId), "output")
      ) {
        warningNodes <- c(warningNodes, dependentId)
      }

      for (evalNodeId in runtime$graphEvalStack) {
        if (!identical(.rtuiGraphNodeType(runtime, evalNodeId), "output")) {
          next
        }
        evalDeps <- .rtuiGraphMapGet(runtime$currentEvalDeps, evalNodeId)
        if (sourceId %in% evalDeps) {
          warningNodes <- c(warningNodes, evalNodeId)
        }
      }

      warningNodes <- unique(warningNodes)
      for (warningNodeId in warningNodes) {
        outputId <- sub("^output:", "", warningNodeId)
        if (!exists(outputId, envir = runtime$selfInvalidationWarnings, inherits = FALSE)) {
          renderName <- "tuiRender*()"
          if (exists(outputId, envir = runtime$outputDefinitions, inherits = FALSE)) {
            definition <- get(outputId, envir = runtime$outputDefinitions, inherits = FALSE)
            if (inherits(definition, "rtuiRenderer")) {
              if (identical(definition$kind, "text")) {
                renderName <- "tuiRenderText()"
              } else if (identical(definition$kind, "numeric")) {
                renderName <- "tuiRenderNumeric()"
              } else if (identical(definition$kind, "table")) {
                renderName <- "tuiRenderTable()"
              }
            }
          }
          warning(
            "Output `", outputId, "` invalidated itself during `", renderName, "` evaluation. ",
            "Avoid side effects in `tuiRender*`; move state updates to `tuiObserve()`/`tuiObserveEvent()`.",
            call. = FALSE
          )
          assign(outputId, TRUE, envir = runtime$selfInvalidationWarnings)
        }
      }
    }

    queue <- c(queue, dependents)
  }

  invisible(NULL)
}

#' Internal helper `.rtuiSetReactiveChanged`.
#'
#' Stores per-run change flags for reactive nodes.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#' @param changed Whether the reactive value changed in this run.
#' @param force If `TRUE`, records `changed` even while isolated.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiSetReactiveChanged <- function(runtime, reactiveId, changed, force = FALSE) {
  if (!isTRUE(force) && .rtuiInIsolate(runtime)) {
    return(invisible(NULL))
  }

  assign(
    reactiveId,
    isTRUE(changed),
    envir = runtime$currentReactiveChanged
  )
}

#' Internal helper `.rtuiGetReactiveChanged`.
#'
#' Retrieves per-run change flags for reactive nodes.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#'
#' @return `TRUE` if the node changed in the current run, otherwise `FALSE`.
#'
#' @keywords internal
#' @noRd
.rtuiGetReactiveChanged <- function(runtime, reactiveId) {
  if (!exists(reactiveId, envir = runtime$currentReactiveChanged, inherits = FALSE)) {
    return(FALSE)
  }
  isTRUE(get(reactiveId, envir = runtime$currentReactiveChanged, inherits = FALSE))
}

#' Internal helper `.rtuiUpdateReactiveState`.
#'
#' Updates reactive state, change flags, and dependent invalidation.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#' @param value Value to evaluate, resolve, or store.
#'
#' @return The updated reactive value.
#'
#' @keywords internal
#' @noRd
.rtuiUpdateReactiveState <- function(runtime, reactiveId, value) {
  previous <- .rtuiReactiveStoreGet(runtime, reactiveId)
  changed <- is.null(previous) || !isTRUE(previous$hasValue) || !identical(previous$value, value)
  .rtuiReactiveStoreSet(runtime, reactiveId, value = value, hasValue = TRUE, dirty = FALSE)
  .rtuiSetReactiveChanged(runtime, reactiveId, changed)
  if (isTRUE(changed)) {
    .rtuiGraphInvalidateDependents(runtime, reactiveId)
  }
  value
}
