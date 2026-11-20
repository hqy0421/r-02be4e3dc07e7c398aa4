# Multi-level BOM explosion and MRP netting --------------------------------

parts <- data.frame(
  part_id = c("P", "S", "L1", "L2", "L3", "L9"),
  unit_cost = c(0, 0, 2, 3, 5, 7),
  on_hand = c(0L, 0L, 25L, 4L, 100L, 10L),
  supplier_id = c(NA, NA, "supA", "supA", "supB", "supC"),
  stringsAsFactors = FALSE
)
# P needs 2 x S and 1 x L3; S needs 3 x L1 and 1 x L2. L9 is never used.
bom <- data.frame(
  parent_id = c("P", "P", "S", "S"),
  child_id = c("S", "L3", "L1", "L2"),
  qty_per = c(2L, 1L, 3L, 1L),
  stringsAsFactors = FALSE
)
orders <- data.frame(
  order_id = c("O1", "O2", "O3"),
  part_id = c("P", "P", "L2"),
  qty = c(1L, 4L, 0L),
  stringsAsFactors = FALSE
)

# -- bom_explode() ----------------------------------------------------------

detail <- bom_explode(orders, bom, parts)
expect_identical(
  detail,
  data.frame(
    order_id = c("O1", "O1", "O1", "O2", "O2", "O2", "O3"),
    part_id = c("L1", "L2", "L3", "L1", "L2", "L3", "L2"),
    demand_qty = c(6L, 2L, 1L, 24L, 8L, 4L, 0L),
    stringsAsFactors = FALSE
  ),
  info = "Shared sub-assemblies are exploded once per order, down to the leaf parts"
)

# Duplicated parent-child lines each contribute, and so does every path to a leaf
parts2 <- data.frame(part_id = c("P", "A", "B", "L"), stringsAsFactors = FALSE)
bom2 <- data.frame(
  parent_id = c("P", "P", "A", "B", "P", "P"),
  child_id = c("A", "B", "L", "L", "L", "L"),
  qty_per = c(1L, 2L, 3L, 1L, 5L, 5L),
  stringsAsFactors = FALSE
)
orders2 <- data.frame(order_id = "O1", part_id = "P", qty = 1L, stringsAsFactors = FALSE)
expect_identical(
  bom_explode(orders2, bom2, parts2)$demand_qty,
  15L,
  info = "Duplicated BOM lines and multiple paths to a leaf all contribute: 3 + 2 + 5 + 5"
)

# An order for a leaf part demands the leaf itself
expect_identical(
  bom_explode(
    data.frame(order_id = "O", part_id = "L1", qty = 7L, stringsAsFactors = FALSE),
    bom, parts
  )$demand_qty,
  7L,
  info = "Ordering a leaf part demands the leaf itself"
)

# A zero-quantity order of a parent part keeps its zero demand for every reachable leaf
expect_identical(
  bom_explode(
    data.frame(order_id = "O", part_id = "P", qty = 0L, stringsAsFactors = FALSE),
    bom, parts
  ),
  data.frame(
    order_id = rep("O", 3L), part_id = c("L1", "L2", "L3"), demand_qty = rep(0L, 3L),
    stringsAsFactors = FALSE
  ),
  info = "Zero-quantity orders of parent parts are preserved"
)

# Several lines of one order accumulate
expect_identical(
  bom_explode(
    data.frame(order_id = c("O", "O"), part_id = c("L1", "L1"), qty = c(2L, 3L), stringsAsFactors = FALSE),
    bom, parts
  )$demand_qty,
  5L,
  info = "Lines sharing an order id accumulate for the order"
)

# -- mrp_plan() --------------------------------------------------------------

plan <- mrp_plan(orders, bom, parts)
expect_identical(
  names(plan),
  c("detail", "order_costs", "leaf_demand", "net_requirements", "supplier_report"),
  info = "mrp_plan() returns all components"
)
expect_identical(plan$detail, detail, info = "The plan detail is the BOM explosion")
expect_identical(class(plan), "poorman_mrp")

expect_identical(
  plan$order_costs,
  data.frame(order_id = c("O1", "O2", "O3"), gross_cost = c(23, 92, 0), stringsAsFactors = FALSE),
  info = "Order gross material cost, keeping the zero-quantity order"
)
expect_identical(
  plan$leaf_demand,
  data.frame(part_id = c("L1", "L2", "L3", "L9"), gross_qty = c(30L, 10L, 5L, 0L), stringsAsFactors = FALSE),
  info = "Leaf gross demand, keeping unused leaf parts"
)
expect_identical(
  plan$net_requirements,
  data.frame(
    part_id = c("L1", "L2", "L3", "L9"),
    on_hand = c(25L, 4L, 100L, 10L),
    gross_qty = c(30L, 10L, 5L, 0L),
    net_qty = c(5L, 6L, 0L, 0L),
    remaining_qty = c(0L, 0L, 95L, 10L),
    stringsAsFactors = FALSE
  ),
  info = "Non-negative net purchase quantities and remaining inventory per leaf part"
)
expect_identical(
  plan$supplier_report,
  data.frame(
    supplier_id = c("supA", "supB", "supC"),
    net_qty = c(11L, 0L, 0L),
    net_cost = c(28, 0, 0),
    stringsAsFactors = FALSE
  ),
  info = "Supplier purchasing report"
)

# The summaries are consistent with the order x leaf detail
expect_identical(
  plan$leaf_demand$gross_qty[match(c("L1", "L2", "L3"), plan$leaf_demand$part_id)],
  as.integer(tapply(detail$demand_qty, detail$part_id, sum)[c("L1", "L2", "L3")]),
  info = "Leaf gross demand is the detail summed per leaf part"
)

# Global inventory is deducted once, never per order: two orders of 5 against 7 on hand
# net to 3 in total, not to 0 + 0
parts_once <- data.frame(
  part_id = c("L"), unit_cost = 1, on_hand = 7L, supplier_id = "s",
  stringsAsFactors = FALSE
)
orders_once <- data.frame(
  order_id = c("O1", "O2"), part_id = "L", qty = c(5L, 5L),
  stringsAsFactors = FALSE
)
plan_once <- mrp_plan(orders_once, bom[0, ], parts_once)
expect_identical(plan_once$leaf_demand$gross_qty, 10L, info = "Gross demand is accumulated over orders")
expect_identical(plan_once$net_requirements$net_qty, 3L, info = "Inventory is deducted once, globally")
expect_identical(plan_once$net_requirements$remaining_qty, 0L, info = "Remaining inventory after the deduction")

# -- Errors -------------------------------------------------------------------

# Cycles are reported, including self-loops
parts_cyc <- data.frame(part_id = c("A", "B"), stringsAsFactors = FALSE)
ord_cyc <- data.frame(order_id = "O", part_id = "A", qty = 1L, stringsAsFactors = FALSE)
expect_error(
  bom_explode(
    ord_cyc,
    data.frame(parent_id = c("A", "B"), child_id = c("B", "A"), qty_per = 1L, stringsAsFactors = FALSE),
    parts_cyc
  ),
  "cycle: A -> B -> A",
  info = "Cycles are an error naming the cycle"
)
expect_error(
  bom_explode(
    ord_cyc,
    data.frame(parent_id = "A", child_id = "A", qty_per = 1L, stringsAsFactors = FALSE),
    parts_cyc
  ),
  "cycle: A -> A",
  info = "Self-loops are an error"
)

# Unknown parts are reported
expect_error(
  bom_explode(
    data.frame(order_id = "O", part_id = "X", qty = 1L, stringsAsFactors = FALSE),
    bom, parts
  ),
  "Unknown parts in `orders\\$part_id`",
  info = "Unknown ordered parts are an error"
)
expect_error(
  bom_explode(orders, transform(bom, parent_id = ifelse(parent_id == "S", "X", parent_id)), parts),
  "Unknown parts in `bom\\$parent_id`",
  info = "Unknown BOM parents are an error"
)
expect_error(
  bom_explode(orders, transform(bom, child_id = ifelse(child_id == "L1", "X", child_id)), parts),
  "Unknown parts in `bom\\$child_id`",
  info = "Unknown BOM children are an error"
)

# Illegal quantities are an error
order_qty <- function(q) data.frame(order_id = "O", part_id = "L1", qty = q, stringsAsFactors = FALSE)
expect_error(bom_explode(order_qty(-1L), bom, parts), "negative", info = "Negative order quantities")
expect_error(bom_explode(order_qty(1.5), bom, parts), "whole-number", info = "Fractional order quantities")
expect_error(bom_explode(order_qty(NA_integer_), bom, parts), "missing", info = "Missing order quantities")
expect_error(bom_explode(order_qty(Inf), bom, parts), "infinite", info = "Infinite order quantities")
expect_error(bom_explode(order_qty("3"), bom, parts), "numeric vector", info = "Character order quantities")
expect_error(bom_explode(order_qty(2^53), bom, parts), "2\\^53", info = "Quantities beyond exact arithmetic")
expect_error(
  bom_explode(orders, transform(bom, qty_per = -1L), parts),
  "negative",
  info = "Negative BOM quantities"
)
expect_error(
  mrp_plan(orders, bom, transform(parts, on_hand = -1L)),
  "negative",
  info = "Negative inventories"
)
expect_error(
  mrp_plan(orders, bom, transform(parts, unit_cost = -1)),
  "negative",
  info = "Negative costs"
)

# Overflow while accumulating is an error
expect_error(
  bom_explode(
    data.frame(order_id = "O", part_id = "P", qty = 1e10, stringsAsFactors = FALSE),
    transform(bom, qty_per = as.numeric(qty_per) * 1e10),
    parts
  ),
  "Overflow",
  info = "Overflow of accumulated quantities"
)
expect_error(
  mrp_plan(
    data.frame(order_id = c("O1", "O2"), part_id = "L1", qty = 5e15, stringsAsFactors = FALSE),
    bom, parts
  ),
  "Overflow",
  info = "Overflow of quantities summed over orders"
)

# Duplicated or missing part identities are an error
expect_error(
  bom_explode(orders, bom, rbind(parts, parts[1L, ])),
  "duplicated ids",
  info = "Duplicated part ids"
)
expect_error(
  bom_explode(transform(orders, part_id = ifelse(part_id == "P", NA, part_id)), bom, parts),
  "missing values",
  info = "Missing part ids"
)

# Part id columns must share one type
expect_error(
  bom_explode(transform(orders, part_id = as.numeric(as.factor(part_id))), bom, parts),
  "same type",
  info = "Mismatched part id types"
)

# A failure delivers no result and leaves the inputs untouched
before_orders <- orders
failed <- try(mrp_plan(order_qty(-1L), bom, parts), silent = TRUE)
expect_true(inherits(failed, "try-error"), info = "Invalid input delivers no result")
expect_identical(orders, before_orders, info = "A failed plan leaves the inputs untouched")

# -- Preservation of identities, types and edge cases -------------------------

# Character, integer and factor identities keep their types
parts_i <- data.frame(
  part_id = 1:4, unit_cost = c(0, 0, 2, 3), on_hand = 0:3,
  supplier_id = c("s1", "s1", "s2", "s2"), stringsAsFactors = FALSE
)
bom_i <- data.frame(
  parent_id = c(1L, 2L), child_id = c(2L, 3L), qty_per = c(2L, 4L),
  stringsAsFactors = FALSE
)
orders_i <- data.frame(
  order_id = c(11L, 12L), part_id = c(1L, 3L), qty = c(1L, 2L),
  stringsAsFactors = FALSE
)
plan_i <- mrp_plan(orders_i, bom_i, parts_i)
expect_true(is.integer(plan_i$detail$order_id), info = "Integer order ids stay integer")
expect_true(is.integer(plan_i$detail$part_id), info = "Integer part ids stay integer")
expect_true(is.integer(plan_i$detail$demand_qty), info = "Quantities are integer when they fit")
expect_true(is.integer(plan_i$net_requirements$on_hand), info = "Inventory keeps its type")
expect_identical(plan_i$leaf_demand$gross_qty, c(10L, 0L), info = "Explosion over integer ids")

lv <- c("P", "S", "L1", "L2", "L3", "L9")
plan_f <- mrp_plan(
  transform(orders, part_id = factor(part_id, levels = lv)),
  transform(bom, parent_id = factor(parent_id, levels = lv), child_id = factor(child_id, levels = lv)),
  transform(parts, part_id = factor(part_id, levels = lv))
)
expect_true(is.factor(plan_f$detail$part_id), info = "Factor part ids stay factors")
expect_identical(levels(plan_f$detail$part_id), lv, info = "Factor levels are preserved")
expect_identical(plan_f$leaf_demand$gross_qty, c(30L, 10L, 5L, 0L), info = "Explosion over factor ids")
expect_error(
  bom_explode(
    transform(orders, part_id = factor(part_id, levels = lv)),
    transform(bom, parent_id = factor(parent_id), child_id = factor(child_id)),
    transform(parts, part_id = factor(part_id, levels = lv))
  ),
  "identical levels",
  info = "Factors with different levels are an error"
)

# Empty inputs stay valid
empty <- mrp_plan(orders[0, ], bom, parts)
expect_identical(nrow(empty$detail), 0L, info = "No orders: empty detail")
expect_identical(nrow(empty$order_costs), 0L, info = "No orders: empty costs")
expect_identical(empty$leaf_demand$gross_qty, rep(0L, 4L), info = "No orders: zero gross demand")
expect_identical(
  empty$net_requirements$remaining_qty, c(25L, 4L, 100L, 10L),
  info = "No orders: all inventory remains"
)

flat <- bom_explode(orders, bom[0, ], parts)
expect_identical(
  flat,
  data.frame(
    order_id = c("O1", "O2", "O3"), part_id = c("P", "P", "L2"),
    demand_qty = c(1L, 4L, 0L), stringsAsFactors = FALSE
  ),
  info = "Without BOM lines every part is a leaf"
)

# Optional columns of parts default when absent
bare <- mrp_plan(orders, bom, parts["part_id"])
expect_identical(bare$order_costs$gross_cost, c(0, 0, 0), info = "Costs default to zero")
expect_identical(bare$net_requirements$net_qty, c(30L, 10L, 5L, 0L), info = "Inventory defaults to zero")
expect_identical(bare$supplier_report$net_qty, 45L, info = "Suppliers default to a single NA group")
expect_true(is.na(bare$supplier_report$supplier_id))
expect_error(
  mrp_plan(orders, bom, parts["part_id"], unit_cost = "price"),
  "no column",
  info = "Explicitly naming a missing column is an error"
)

# Suppliers may be missing; they form their own group
plan_na <- mrp_plan(orders, bom, transform(parts, supplier_id = ifelse(part_id == "L1", NA, supplier_id)))
expect_identical(
  plan_na$supplier_report,
  data.frame(
    supplier_id = c("supA", "supB", "supC", NA),
    net_qty = c(6L, 0L, 0L, 5L),
    net_cost = c(18, 0, 0, 10),
    stringsAsFactors = FALSE
  ),
  info = "Missing suppliers form their own group"
)

# Column names are arguments
plan_c <- mrp_plan(
  data.frame(oid = "A", sku = "P", n = 2L, stringsAsFactors = FALSE),
  data.frame(up = "P", down = "L1", per = 3L, stringsAsFactors = FALSE),
  data.frame(
    sku = c("P", "L1"), price = c(0, 4), stock = 5L, vendor = c("v1", "v2"),
    stringsAsFactors = FALSE
  ),
  order_id = "oid", part_id = "sku", qty = "n",
  parent_id = "up", child_id = "down", qty_per = "per",
  unit_cost = "price", on_hand = "stock", supplier_id = "vendor"
)
expect_identical(names(plan_c$detail), c("oid", "sku", "demand_qty"), info = "Detail keeps input column names")
expect_identical(
  names(plan_c$net_requirements), c("sku", "stock", "gross_qty", "net_qty", "remaining_qty"),
  info = "Net requirements keep input column names"
)
expect_identical(plan_c$order_costs$gross_cost, 24, info = "Costs over renamed columns")
expect_identical(plan_c$net_requirements$net_qty, 1L, info = "Netting over renamed columns")

# Result column names must not clash with input column names
expect_error(
  bom_explode(
    data.frame(demand_qty = "O", part_id = "L1", qty = 1L, stringsAsFactors = FALSE),
    bom, parts, order_id = "demand_qty"
  ),
  "clash",
  info = "Clashing column names are an error"
)

# One column cannot play several roles
expect_error(
  bom_explode(orders, bom, parts, qty = "part_id"),
  "different columns",
  info = "The column roles of a table must be distinct"
)
