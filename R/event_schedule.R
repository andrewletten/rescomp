#' Define a schedule of an event occurring at a fixed set of times
#'
#' Produces an event schedule object suitable for passing in the `events` argument to `spec_rescomp()`.
#'
#' @param event_obj An object of class `rescomp_event`.
#' @param times A numeric vector of times at which the event should occur.
#' @param priority A number. If two events occur at the same time, the event with the lower priority number is processed first.
#'
#' @returns S3 object of class `rescomp_event_schedule`.
#' @export
#'
#' @examples
#' # TODO
event_schedule_fixed <- function(event_obj, times, priority = 0) {
  schedule <- list(event_obj = event_obj, times = times, priority = priority)
  class(schedule) <- c("rescomp_event_schedule_fixed", "rescomp_event_schedule")
  return(schedule)
}

#' Define a schedule of an event occurring repeatedly at a fixed period
#'
#' Produces an event schedule object suitable for passing in the `events` argument to `spec_rescomp()`.
#'
#' @param event_obj An object of class `rescomp_event`.
#' @param period A number: the period at which the event occurs.
#' @param start_time A number: the time at which the event first occurs.
#' @param priority A number. If two events occur at the same time, the event with the lower priority number is processed first.
#'
#' @returns S3 object of class `rescomp_event_schedule`.
#' @export
#'
#' @examples
#' # TODO
event_schedule_periodic <- function(event_obj, period, start_time = period, priority = 0) {
  schedule <- list(event_obj = event_obj, period = period, start_time = start_time, priority = priority)
  class(schedule) <- c("rescomp_event_schedule_periodic", "rescomp_event_schedule")
  return(schedule)
}

#' Gets the list of times from a `rescomp_event_schedule` object
#'
#' This function is normally only for internal use, but is exported to aid users in debugging their created `rescomp_event` objects.
#'
#' @param event_schedule_obj An object of class `rescomp_event_schedule`.
#' @param totaltime The total time which the simulation will run for.
#'
#' @returns A vector of times.
#' @export
#'
#' @examples
#' # TODO
get_event_times <- function(event_schedule_obj, totaltime) {
  UseMethod("get_event_times")
}

#' @export
get_event_times.rescomp_event_schedule_fixed <- function(event_schedule_obj, totaltime) {
  return(event_schedule_obj$times)
}

#' @export
get_event_times.rescomp_event_schedule_periodic <- function(event_schedule_obj, totaltime) {
  return(seq(from = event_schedule_obj$start_time, to = totaltime, by = event_schedule_obj$period))
}

#' Prepares a sorted data frame of times of events
#'
#' Produces a data frame to be used by the event function passed to deSolve
#'
#' @param event_schedule_list A list of `rescomp_event_schedule` objects as passed to `spec_rescomp()`.
#' @param totaltime The total time which the simulation will run for.
#'
#' @returns A data frame with three columns: time, priority, and event_index.
#'     `time` is the time at which the events should occur,
#'     `priority` breaks ties (lower priority first) in time, and
#'     `event_index` is the index in the original event schedule list of the corresponding event.
#'     The data frame will be sorted by ascending time, with ties broken by ascending priority.
#' @noRd
prepare_event_schedule_df <- function(event_schedule_list, totaltime) {
  if (length(event_schedule_list) == 0) {
    return(data.frame(time = c(), priority = c(), event_index = c()))
  }
  dat <- do.call(rbind, lapply(seq_along(event_schedule_list), function(i) {
    data.frame(
      time = get_event_times(event_schedule_list[[i]], totaltime),
      priority = event_schedule_list[[i]]$priority,
      event_index = i
    )
  }))
  dat <- dat[order(dat$time, dat$priority), ]
  # TODO: Warn of colliding time & priority.
  return(dat)
}

propagate_crnum.rescomp_event_schedule <- function(obj, spnum, resnum) {
  obj$event_obj <- propagate_crnum(obj$event_obj, spnum, resnum)
  return(obj)
}
