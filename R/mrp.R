#' Explode orders through a multi-level bill of materials
#'
#' @description
#' `bom_explode()` walks every order line down a multi-level, acyclic bill of materials (BOM) and accumulates the
#' true demand of each order for each leaf part, where a leaf part is a part which never appears as a parent in
#' `bom`. The result is the order x leaf detail: one row per order and leaf part with the total quantity the order
#' requires of that leaf part.
#'
#' * Sub-assemblies shared by several orders are exploded once per order, so every order is charged its full
#'   demand; nothing is deduplicated across orders.
#' * Duplicated parent-child lines of `bom` each contribute their quantity.
#' * A leaf part reached by several paths accumulates the demand of every path.
#' * A cycle in `bom`, or a part of `orders` or `bom` which is not listed in `parts`, is an error.
#'
#' @param orders A `data.frame` of order lines. By default it is expected to have the columns `order_id`, `part_id`
#' and `qty`; see the `order_id`, `part_id` and `qty` arguments. `qty` must be a whole-number, non-negative
#' quantity; zero-quantity orders are kept in the results. Several lines may share an `order_id`, in which case
#' their demands are accumulated for that order.
#' @param bom A `data.frame` of usage lines, one row per parent-child usage, with the columns `parent_id`,
#' `child_id` and `qty_per` by default. `qty_per` is the whole-number, non-negative quantity of the child part
#' needed for one unit of the parent part. Duplicated parent-child lines are allowed and each contributes.
#' @param parts A `data.frame` of the stable part identities, with a unique `part_id` column by default. Every part
#' referred to by `orders` or `bom` must be listed here.
#' @param order_id,part_id,qty,parent_id,child_id,qty_per `character(1)`. The names of the columns of `orders`,
#' `bom` and `parts` to use. The part id columns of `orders`, `bom` and `parts` must all have the same type
#' (character, numeric, or factor with identical levels); their type is preserved in the results.
#'
#' @return
#' A `data.frame` with one row per order and leaf part, with the columns `order_id`, `part_id` (the leaf part) and
#' `demand_qty`, the total quantity of the leaf part required by the order. `demand_qty` is an integer vector when
#' every quantity fits in one, and a double vector of whole numbers otherwise. Quantities are accumulated exactly;
#' an accumulated quantity of 2^53 or more is an overflow error.
#'
#' @examples
#' parts <- data.frame(
#'   part_id = c("bike", "wheel", "tire", "tube"),
#'   stringsAsFactors = FALSE
#' )
#' bom <- data.frame(
#'   parent_id = c("bike", "wheel", "wheel"),
#'   child_id = c("wheel", "tire", "tube"),
#'   qty_per = c(2L, 1L, 1L),
#'   stringsAsFactors = FALSE
#' )
#' orders <- data.frame(
#'   order_id = c("o1", "o2"),
#'   part_id = c("bike", "bike"),
#'   qty = c(3L, 5L),
#'   stringsAsFactors = FALSE
#' )
#' bom_explode(orders, bom, parts)
#'
#' @export
bom_explode <- function(orders, bom, parts,
                        order_id = "order_id", part_id = "part_id", qty = "qty",
                        parent_id = "parent_id", child_id = "child_id", qty_per = "qty_per") {
  prep <- mrp_prepare(
    orders, bom, parts,
    order_id = order_id, part_id = part_id, qty = qty,
    parent_id = parent_id, child_id = child_id, qty_per = qty_per
  )
  mrp_check_out_names(c(order_id, part_id, "demand_qty"))
  detail <- mrp_detail(prep)
  detail$demand_qty <- mrp_qty_out(detail$demand_qty)
  names(detail) <- c(order_id, part_id, "demand_qty")
  detail
}

#' Material requirements planning over a multi-level bill of materials
#'
#' @description
#' `mrp_plan()` explodes the order lines through the bill of materials with [bom_explode()] and consumes the
#' resulting order x leaf detail with poorman's native joins and grouping to produce:
#'
#' * `detail`: the order x leaf part detail, as returned by [bom_explode()].
#' * `order_costs`: the gross material cost of every order, the sum over the leaf parts of the demanded quantity
#'   times the leaf part's `unit_cost`. Every order is kept, including zero-quantity orders.
#' * `leaf_demand`: the gross demand of every leaf part, summed over the orders. Leaf parts with no demand are
#'   kept with a gross quantity of zero.
#' * `net_requirements`: for every leaf part, its `on_hand` inventory, its gross demand, the non-negative net
#'   purchase quantity `net_qty = pmax(0, gross_qty - on_hand)` and the remaining inventory
#'   `remaining_qty = pmax(0, on_hand - gross_qty)`. The global inventory of each leaf part is deducted exactly
#'   once, from the total gross demand of all orders, never per order.
#' * `supplier_report`: the net purchase quantity and net purchase cost of every supplier, summed over their leaf
#'   parts.
#'
#' All components are computed from the order x leaf detail or nothing is returned at all: invalid quantities,
#' unknown parts, cycles, overflow and any other failure are errors and deliver no result.
#'
#' @inheritParams bom_explode
#' @param unit_cost,on_hand,supplier_id `character(1)`. The names of optional columns of `parts`, used for the
#' leaf parts: `unit_cost`, a non-negative numeric cost; `on_hand`, the whole-number, non-negative global
#' inventory; and `supplier_id`, the supplier identity. When an argument is left at its default and `parts` has no
#' such column, a default is used: a cost of 0, an inventory of 0 and an `NA` supplier. Naming a column which
#' `parts` does not have is an error.
#'
#' @return
#' A `list` with class `"poorman_mrp"` and the components `detail`, `order_costs`, `leaf_demand`,
#' `net_requirements` and `supplier_report` described in the section above. Identity columns keep the names and
#' types of the input columns.
#'
#' @examples
#' parts <- data.frame(
#'   part_id = c("bike", "wheel", "tire", "tube"),
#'   unit_cost = c(0, 0, 20, 5),
#'   on_hand = c(0L, 0L, 10L, 50L),
#'   supplier_id = c(NA, NA, "rubber_co", "rubber_co"),
#'   stringsAsFactors = FALSE
#' )
#' bom <- data.frame(
#'   parent_id = c("bike", "wheel", "wheel"),
#'   child_id = c("wheel", "tire", "tube"),
#'   qty_per = c(2L, 1L, 1L),
#'   stringsAsFactors = FALSE
#' )
#' orders <- data.frame(
#'   order_id = c("o1", "o2"),
#'   part_id = c("bike", "bike"),
#'   qty = c(3L, 5L),
#'   stringsAsFactors = FALSE
#' )
#' plan <- mrp_plan(orders, bom, parts)
#' plan$order_costs
#' plan$leaf_demand
#' plan$net_requirements
#' plan$supplier_report
#'
#' @export
mrp_plan <- function(orders, bom, parts,
                     order_id = "order_id", part_id = "part_id", qty = "qty",
                     parent_id = "parent_id", child_id = "child_id", qty_per = "qty_per",
                     unit_cost = "unit_cost", on_hand = "on_hand", supplier_id = "supplier_id") {
  cost_given <- !missing(unit_cost)
  hand_given <- !missing(on_hand)
  supp_given <- !missing(supplier_id)
  prep <- mrp_prepare(
    orders, bom, parts,
    order_id = order_id, part_id = part_id, qty = qty,
    parent_id = parent_id, child_id = child_id, qty_per = qty_per
  )

  n_parts <- length(prep$part_ids)
  cost <- mrp_optional_col(parts, unit_cost, "parts", "unit_cost", cost_given)
  if (is.null(cost)) cost <- rep(0, n_parts) else mrp_check_cost(cost, "parts", unit_cost)
  hand <- mrp_optional_col(parts, on_hand, "parts", "on_hand", hand_given)
  if (is.null(hand)) hand <- rep(0L, n_parts) else mrp_check_qty(hand, "parts", on_hand)
  supp <- mrp_optional_col(parts, supplier_id, "parts", "supplier_id", supp_given)
  if (is.null(supp)) supp <- rep(NA_character_, n_parts) else mrp_check_supplier(supp, "parts", supplier_id)

  mrp_check_out_names(c(order_id, part_id, "demand_qty"))
  mrp_check_out_names(c(order_id, "gross_cost"))
  mrp_check_out_names(c(part_id, "gross_qty"))
  mrp_check_out_names(c(part_id, on_hand, "gross_qty", "net_qty", "remaining_qty"))
  mrp_check_out_names(c(supplier_id, "net_qty", "net_cost"))

  # The order x leaf detail every component below is computed from
  detail <- mrp_detail(prep)

  is_leaf <- prep$is_leaf
  leaf_ids <- prep$part_ids[is_leaf]
  n_leaves <- length(leaf_ids)

  # Gross demand per leaf part, grouped from the detail
  if (nrow(detail) > 0L) {
    gross <- detail %>% group_by(.part) %>% summarise(gross_qty = sum(demand_qty), .groups = "drop")
    mrp_check_overflow(gross$gross_qty, "gross demand")
  } else {
    gross <- new_data_frame(list(.part = leaf_ids[0L], gross_qty = double()), 0L)
  }

  # Gross material cost per order: leaf unit costs joined onto the detail, then summed per order
  cost_map <- new_data_frame(list(.part = leaf_ids, .cost = cost[is_leaf]), n_leaves)
  costed <- left_join(detail, cost_map, by = ".part")
  line_cost <- costed$demand_qty * costed$.cost
  mrp_check_cost_overflow(line_cost)
  costed$.line_cost <- line_cost
  if (nrow(costed) > 0L) {
    cost_by_order <- costed %>% group_by(.order) %>% summarise(gross_cost = sum(.line_cost), .groups = "drop")
    mrp_check_cost_overflow(cost_by_order$gross_cost)
  } else {
    cost_by_order <- new_data_frame(list(.order = prep$orders$.order[0L], gross_cost = double()), 0L)
  }
  order_ids <- distinct(new_data_frame(list(.order = prep$orders$.order), nrow(prep$orders)))
  order_costs <- left_join(order_ids, cost_by_order, by = ".order")
  order_costs$gross_cost[is.na(order_costs$gross_cost)] <- 0

  # Gross demand of every leaf part, keeping unused leaf parts at zero
  leaf_demand <- left_join(new_data_frame(list(.part = leaf_ids), n_leaves), gross, by = ".part")
  gross_qty <- leaf_demand$gross_qty
  gross_qty[is.na(gross_qty)] <- 0

  # Net requirements: the global inventory of each leaf part is deducted exactly once
  net <- left_join(new_data_frame(list(.part = leaf_ids, .hand = hand[is_leaf]), n_leaves), gross, by = ".part")
  net_gross <- net$gross_qty
  net_gross[is.na(net_gross)] <- 0
  hand_num <- as.double(net$.hand)
  net_qty <- pmax(0, net_gross - hand_num)
  remaining_qty <- pmax(0, hand_num - net_gross)

  # Purchasing report per supplier
  sup <- new_data_frame(list(.sup = supp[is_leaf], .net = net_qty, .cost = cost[is_leaf]), n_leaves)
  sup$.net_cost <- sup$.net * sup$.cost
  mrp_check_cost_overflow(sup$.net_cost)
  if (n_leaves > 0L) {
    supplier_report <- sup %>%
      group_by(.sup) %>%
      summarise(net_qty = sum(.net), net_cost = sum(.net_cost), .groups = "drop")
    mrp_check_cost_overflow(supplier_report$net_cost)
  } else {
    supplier_report <- new_data_frame(list(.sup = supp[0L], net_qty = double(), net_cost = double()), 0L)
  }

  detail$demand_qty <- mrp_qty_out(detail$demand_qty)
  names(detail) <- c(order_id, part_id, "demand_qty")
  names(order_costs) <- c(order_id, "gross_cost")
  leaf_demand <- new_data_frame(list(leaf_ids, mrp_qty_out(gross_qty)), n_leaves)
  names(leaf_demand) <- c(part_id, "gross_qty")
  net <- new_data_frame(
    list(leaf_ids, net$.hand, mrp_qty_out(net_gross), mrp_qty_out(net_qty), mrp_qty_out(remaining_qty)),
    n_leaves
  )
  names(net) <- c(part_id, on_hand, "gross_qty", "net_qty", "remaining_qty")
  supplier_report$net_qty <- mrp_qty_out(supplier_report$net_qty)
  names(supplier_report) <- c(supplier_id, "net_qty", "net_cost")

  structure(
    list(
      detail = detail,
      order_costs = order_costs,
      leaf_demand = leaf_demand,
      net_requirements = net,
      supplier_report = supplier_report
    ),
    class = "poorman_mrp"
  )
}

#' @export
print.poorman_mrp <- function(x, ...) {
  cat("# A poorman MRP plan\n")
  for (nm in names(x)) {
    cat("\n## ", nm, "\n", sep = "")
    print(x[[nm]], ...)
  }
  invisible(x)
}

# -- Helpers -------------------------------------------------------------------

#' The largest accumulated quantity: exact whole-number arithmetic in doubles needs values below 2^53
#' @noRd
MRP_MAX_QTY <- 2^53

#' Validate the structural inputs and return them in a canonical form
#'
#' All checks are run before any quantity is computed, so that a failure delivers no result. The returned `list`
#' has `orders` (`data.frame` with `.order`, `.part`, `.qty`), `bom` (`data.frame` with `.parent`, `.child`,
#' `.qty_per`), `part_ids` (the part id column of `parts`) and `is_leaf` (which parts are never a parent).
#' @noRd
mrp_prepare <- function(orders, bom, parts, order_id, part_id, qty, parent_id, child_id, qty_per) {
  mrp_check_table(orders, "orders")
  mrp_check_table(bom, "bom")
  mrp_check_table(parts, "parts")

  if (anyDuplicated(c(order_id, part_id, qty))) {
    stop("The `order_id`, `part_id` and `qty` arguments must name different columns of `orders`.", call. = FALSE)
  }
  if (anyDuplicated(c(parent_id, child_id, qty_per))) {
    stop("The `parent_id`, `child_id` and `qty_per` arguments must name different columns of `bom`.", call. = FALSE)
  }

  o_order <- mrp_take_col(orders, order_id, "orders", "order_id")
  o_part <- mrp_take_col(orders, part_id, "orders", "part_id")
  o_qty <- mrp_take_col(orders, qty, "orders", "qty")
  b_parent <- mrp_take_col(bom, parent_id, "bom", "parent_id")
  b_child <- mrp_take_col(bom, child_id, "bom", "child_id")
  b_qty <- mrp_take_col(bom, qty_per, "bom", "qty_per")
  p_id <- mrp_take_col(parts, part_id, "parts", "part_id")

  mrp_check_id(p_id, "parts", part_id)
  mrp_check_id(o_order, "orders", order_id)
  mrp_check_id(o_part, "orders", part_id)
  mrp_check_id(b_parent, "bom", parent_id)
  mrp_check_id(b_child, "bom", child_id)

  dup <- unique(p_id[duplicated(p_id)])
  if (length(dup) > 0L) {
    stop(
      "`parts$", part_id, "` contains duplicated ids: ", mrp_format_values(dup),
      ". Part identities must be stable and unique.",
      call. = FALSE
    )
  }

  mrp_check_same_part_type(p_id, o_part, b_parent, b_child, part_id, parent_id, child_id)

  mrp_check_qty(o_qty, "orders", qty)
  mrp_check_qty(b_qty, "bom", qty_per)

  mrp_check_known(o_part, p_id, "orders", part_id)
  mrp_check_known(b_parent, p_id, "bom", parent_id)
  mrp_check_known(b_child, p_id, "bom", child_id)

  from <- match(b_parent, p_id)
  to <- match(b_child, p_id)
  if (length(from) > 0L) {
    cycle <- mrp_find_cycle(from, to)
    if (length(cycle) > 0L) {
      stop(
        "The BOM in `bom` contains a cycle: ",
        paste(as.character(p_id[cycle]), collapse = " -> "),
        ". The BOM must be acyclic.",
        call. = FALSE
      )
    }
  }

  list(
    orders = new_data_frame(list(.order = o_order, .part = o_part, .qty = as.double(o_qty)), length(o_order)),
    bom = new_data_frame(list(.parent = b_parent, .child = b_child, .qty_per = as.double(b_qty)), length(b_parent)),
    part_ids = p_id,
    is_leaf = !(seq_along(p_id) %in% from)
  )
}

#' Explode the orders to the order x leaf detail
#'
#' The frontier of unexpanded requirements is joined onto `bom` with poorman's native `inner_join()` until only
#' leaf parts remain; duplicated usage lines and multiple paths each contribute their rows. The leaf rows are then
#' summed per order and leaf part with the native `group_by()` and `summarise()`.
#' @noRd
mrp_detail <- function(prep) {
  bom <- prep$bom
  parents <- unique(bom$.parent)
  frontier <- prep$orders
  acc <- list()
  iterations <- 0L
  max_iterations <- length(prep$part_ids) + 1L
  repeat {
    is_leaf <- !(frontier$.part %in% parents)
    if (any(is_leaf)) acc[[length(acc) + 1L]] <- frontier[is_leaf, , drop = FALSE]
    frontier <- frontier[!is_leaf, , drop = FALSE]
    if (nrow(frontier) == 0L) break
    iterations <- iterations + 1L
    if (iterations > max_iterations) {
      stop("The BOM could not be exploded completely; it may contain a cycle.", call. = FALSE)
    }
    frontier <- inner_join(frontier, bom, by = c(".part" = ".parent"))
    qty <- frontier$.qty * frontier$.qty_per
    mrp_check_overflow(qty)
    frontier <- new_data_frame(
      list(.order = frontier$.order, .part = frontier$.child, .qty = qty),
      length(qty)
    )
  }
  if (length(acc) == 0L) {
    return(new_data_frame(
      list(.order = prep$orders$.order[0L], .part = prep$part_ids[0L], demand_qty = double()),
      0L
    ))
  }
  raw <- bind_rows(acc)
  detail <- raw %>% group_by(.order, .part) %>% summarise(demand_qty = sum(.qty), .groups = "drop")
  mrp_check_overflow(detail$demand_qty)
  detail
}

#' Find a cycle in a directed graph
#'
#' @param from,to `integer(n)`. The edges, as node numbers.
#' @return An `integer` vector of node numbers forming a cycle, ending where it starts, or `integer(0)` when the
#' graph is acyclic.
#' @noRd
mrp_find_cycle <- function(from, to) {
  keep <- !duplicated(paste(from, to, sep = "\r"))
  from <- from[keep]
  to <- to[keep]
  nodes <- sort(unique(c(from, to)))
  adjacency <- split(to, from)
  state <- integer(length(nodes))
  names(state) <- as.character(nodes)
  for (start in nodes) {
    if (state[as.character(start)] != 0L) next
    node_stack <- start
    iter_stack <- 1L
    state[as.character(start)] <- 1L
    while (length(node_stack) > 0L) {
      top <- node_stack[length(node_stack)]
      children <- adjacency[[as.character(top)]]
      i <- iter_stack[length(iter_stack)]
      if (is.null(children) || i > length(children)) {
        state[as.character(top)] <- 2L
        node_stack <- node_stack[-length(node_stack)]
        iter_stack <- iter_stack[-length(iter_stack)]
      } else {
        iter_stack[length(iter_stack)] <- i + 1L
        child <- children[i]
        child_state <- state[as.character(child)]
        if (child_state == 1L) {
          pos <- match(child, node_stack)
          return(c(node_stack[pos:length(node_stack)], child))
        }
        if (child_state == 0L) {
          state[as.character(child)] <- 1L
          node_stack <- c(node_stack, child)
          iter_stack <- c(iter_stack, 1L)
        }
      }
    }
  }
  integer(0L)
}

# -- Checking helpers ----------------------------------------------------------

mrp_is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

mrp_check_table <- function(x, name) {
  if (!is.data.frame(x)) stop("`", name, "` must be a data.frame.", call. = FALSE)
}

#' Fetch a required column, checking its name was given as a single string
#' @noRd
mrp_take_col <- function(df, col, df_name, arg) {
  if (!mrp_is_string(col)) stop("`", arg, "` must be the name of a column of `", df_name, "`.", call. = FALSE)
  if (!(col %in% names(df))) {
    stop("`", df_name, "` has no column \"", col, "\" (see argument `", arg, "`).", call. = FALSE)
  }
  .subset2(df, col)
}

#' Fetch an optional column of `parts`: `NULL` when absent, unless it was explicitly asked for
#' @noRd
mrp_optional_col <- function(df, col, df_name, arg, given) {
  if (!mrp_is_string(col)) stop("`", arg, "` must be the name of a column of `", df_name, "`.", call. = FALSE)
  if (col %in% names(df)) return(.subset2(df, col))
  if (given) stop("`", df_name, "` has no column \"", col, "\" (see argument `", arg, "`).", call. = FALSE)
  NULL
}

mrp_check_id <- function(x, df_name, col) {
  what <- sprintf("`%s$%s`", df_name, col)
  if (!is.null(dim(x)) || !(is.character(x) || is.numeric(x) || is.factor(x))) {
    stop(what, " must be a character, numeric or factor vector of ids.", call. = FALSE)
  }
  if (anyNA(x)) stop(what, " must not contain missing values.", call. = FALSE)
}

mrp_check_supplier <- function(x, df_name, col) {
  what <- sprintf("`%s$%s`", df_name, col)
  if (!is.null(dim(x)) || !(is.character(x) || is.numeric(x) || is.factor(x))) {
    stop(what, " must be a character, numeric or factor vector of supplier ids.", call. = FALSE)
  }
}

#' Check a whole-number, non-negative quantity column
#' @noRd
mrp_check_qty <- function(x, df_name, col) {
  what <- sprintf("`%s$%s`", df_name, col)
  if (!is.null(dim(x)) || is.object(x) || !(is.integer(x) || is.double(x))) {
    stop(what, " must be a numeric vector of non-negative whole-number quantities.", call. = FALSE)
  }
  if (anyNA(x)) stop(what, " must not contain missing quantities.", call. = FALSE)
  if (any(is.infinite(x))) stop(what, " must not contain infinite quantities.", call. = FALSE)
  if (any(x < 0)) stop(what, " must not contain negative quantities.", call. = FALSE)
  if (any(x >= MRP_MAX_QTY)) stop(what, " must be smaller than 2^53.", call. = FALSE)
  if (any(x != floor(x))) stop(what, " must contain whole-number quantities.", call. = FALSE)
}

mrp_check_cost <- function(x, df_name, col) {
  what <- sprintf("`%s$%s`", df_name, col)
  if (!is.null(dim(x)) || is.object(x) || !(is.integer(x) || is.double(x))) {
    stop(what, " must be a numeric vector of non-negative costs.", call. = FALSE)
  }
  if (anyNA(x)) stop(what, " must not contain missing costs.", call. = FALSE)
  if (any(is.infinite(x))) stop(what, " must not contain infinite costs.", call. = FALSE)
  if (any(x < 0)) stop(what, " must not contain negative costs.", call. = FALSE)
}

#' Check the part id columns of the three tables have the same type
#' @noRd
mrp_check_same_part_type <- function(p_id, o_part, b_parent, b_child, part_id, parent_id, child_id) {
  cols <- list(p_id, o_part, b_parent, b_child)
  labels <- c(
    sprintf("`parts$%s`", part_id), sprintf("`orders$%s`", part_id),
    sprintf("`bom$%s`", parent_id), sprintf("`bom$%s`", child_id)
  )
  kinds <- vapply(cols, mrp_id_kind, "")
  if (length(unique(kinds)) > 1L) {
    stop(
      "Part id columns must all have the same type, but ",
      paste0(labels, " is <", kinds, ">", collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (kinds[1L] == "factor") {
    lev <- levels(p_id)
    for (i in 2:4) {
      if (!identical(levels(cols[[i]]), lev)) {
        stop(
          "Factor part id columns must have identical levels, but ", labels[[1L]], " and ", labels[[i]],
          " differ. Convert them to character or align their levels.",
          call. = FALSE
        )
      }
    }
  }
}

mrp_id_kind <- function(x) {
  if (is.factor(x)) return("factor")
  cls <- attr(x, "class")
  if (!is.null(cls)) paste(cls, collapse = "/") else typeof(x)
}

#' Check all parts of a column are listed in the part ids
#' @noRd
mrp_check_known <- function(x, ids, df_name, col) {
  unknown <- unique(x[!(x %in% ids)])
  if (length(unknown) > 0L) {
    stop(
      "Unknown parts in `", df_name, "$", col, "`: ", mrp_format_values(unknown),
      ". Every part must be listed in `parts`.",
      call. = FALSE
    )
  }
}

mrp_format_values <- function(x) {
  n <- length(x)
  shown <- paste0("\"", as.character(x[seq_len(min(n, 5L))]), "\"", collapse = ", ")
  if (n > 5L) shown <- paste0(shown, " and ", n - 5L, " more")
  shown
}

#' Check accumulated quantities remain exact whole numbers
#' @noRd
mrp_check_overflow <- function(x, what = "quantity") {
  if (any(!is.finite(x)) || any(x >= MRP_MAX_QTY)) {
    stop("Overflow: the accumulated ", what, " reaches 2^53 or more.", call. = FALSE)
  }
}

mrp_check_cost_overflow <- function(x) {
  if (any(!is.finite(x))) stop("Overflow: material costs exceed the largest representable number.", call. = FALSE)
}

#' Check result column names do not clash with input column names
#' @noRd
mrp_check_out_names <- function(nms) {
  if (anyDuplicated(nms)) {
    stop(
      "Column names of the inputs clash with result column names: ",
      paste0("\"", unique(nms[duplicated(nms)]), "\"", collapse = ", "),
      ". Please rename the clashing columns.",
      call. = FALSE
    )
  }
}

#' Give quantities their result type: integer where every value fits, double of whole numbers otherwise
#' @noRd
mrp_qty_out <- function(x) {
  if (all(x <= .Machine$integer.max)) return(as.integer(x))
  x
}
