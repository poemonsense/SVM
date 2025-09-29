package svm

import chisel3._
import chisel3.util._

abstract class ReplacementPolicy {
  def nBits: Int
  def perSet: Boolean
  def way: UInt
  def miss: Unit
  def hit: Unit
  def access(touch_way: UInt): Unit
  def access(touch_ways: Seq[Valid[UInt]]): Unit
  def state_read: UInt
  def get_next_state(state: UInt, touch_way: UInt): UInt
  def get_next_state(state: UInt, touch_ways: Seq[Valid[UInt]]): UInt = {
    touch_ways.foldLeft(state)((prev, touch_way) => Mux(touch_way.valid, get_next_state(prev, touch_way.bits), prev))
  }
  def get_replace_way(state: UInt): UInt
  def get_replace_way(state: UInt, cond: UInt): UInt
}

object ReplacementPolicy {
  def fromString(s: String, n_ways: Int): Option[ReplacementPolicy] = {
    s match {
      case "plru" => Some(new PseudoLRU(n_ways))
      case "none" => None
      case _      => throw new RuntimeException(s"unknown type $s")
    }
  }
}

class PseudoLRU(n_ways: Int) extends ReplacementPolicy {
  // Pseudo-LRU tree algorithm: https://en.wikipedia.org/wiki/Pseudo-LRU#Tree-PLRU
  //
  //
  // - bits storage example for 4-way PLRU binary tree:
  //                  bit[2]: ways 3+2 older than ways 1+0
  //                  /                                  \
  //     bit[1]: way 3 older than way 2    bit[0]: way 1 older than way 0
  //
  //
  // - bits storage example for 3-way PLRU binary tree:
  //                  bit[1]: way 2 older than ways 1+0
  //                                                  \
  //                                       bit[0]: way 1 older than way 0
  //
  //
  // - bits storage example for 8-way PLRU binary tree:
  //                      bit[6]: ways 7-4 older than ways 3-0
  //                      /                                  \
  //            bit[5]: ways 7+6 > 5+4                bit[2]: ways 3+2 > 1+0
  //            /                    \                /                    \
  //     bit[4]: way 7>6    bit[3]: way 5>4    bit[1]: way 3>2    bit[0]: way 1>0

  def nBits = n_ways - 1
  def perSet = true
  private val state_reg = if (nBits == 0) Reg(UInt(0.W)) else RegInit(0.U(nBits.W))
  def state_read = WireDefault(state_reg)

  def access(touch_way: UInt): Unit = {
    state_reg := get_next_state(state_reg, touch_way)
  }
  def access(touch_ways: Seq[Valid[UInt]]): Unit = {
    when(VecInit(touch_ways.map(_.valid)).asUInt.orR) {
      state_reg := get_next_state(state_reg, touch_ways)
    }
  }

  /** @param state state_reg bits for this sub-tree
   * @param touch_way touched way encoded value bits for this sub-tree
   * @param tree_nways number of ways in this sub-tree
   */
  def get_next_state(state: UInt, touch_way: UInt, tree_nways: Int): UInt = {
    require(state.getWidth == (tree_nways - 1), s"wrong state bits width ${state.getWidth} for $tree_nways ways")
    require(
      touch_way.getWidth == (log2Ceil(tree_nways).max(1)),
      s"wrong encoded way width ${touch_way.getWidth} for $tree_nways ways",
    )

    if (tree_nways > 2) {
      // we are at a branching node in the tree, so recurse
      val right_nways: Int = 1 << (log2Ceil(tree_nways) - 1) // number of ways in the right sub-tree
      val left_nways: Int = tree_nways - right_nways // number of ways in the left sub-tree
      val set_left_older = !touch_way(log2Ceil(tree_nways) - 1)
      val left_subtree_state = state(tree_nways - 3, right_nways - 1)
      val right_subtree_state = state(right_nways - 2, 0)

      if (left_nways > 1) {
        // we are at a branching node in the tree with both left and right sub-trees, so recurse both sub-trees
        Cat(
          set_left_older,
          Mux(
            set_left_older,
            left_subtree_state, // if setting left sub-tree as older, do NOT recurse into left sub-tree
            get_next_state(left_subtree_state, touch_way(log2Ceil(left_nways) - 1, 0), left_nways),
          ), // recurse left if newer
          Mux(
            set_left_older,
            get_next_state(right_subtree_state, touch_way(log2Ceil(right_nways) - 1, 0), right_nways), // recurse right if newer
            right_subtree_state,
          ),
        ) // if setting right sub-tree as older, do NOT recurse into right sub-tree
      } else {
        // we are at a branching node in the tree with only a right sub-tree, so recurse only right sub-tree
        Cat(
          set_left_older,
          Mux(
            set_left_older,
            get_next_state(right_subtree_state, touch_way(log2Ceil(right_nways) - 1, 0), right_nways), // recurse right if newer
            right_subtree_state,
          ),
        ) // if setting right sub-tree as older, do NOT recurse into right sub-tree
      }
    } else if (tree_nways == 2) {
      // we are at a leaf node at the end of the tree, so set the single state bit opposite of the lsb of the touched way encoded value
      !touch_way(0)
    } else { // tree_nways <= 1
      // we are at an empty node in an empty tree for 1 way, so return single zero bit for Chisel (no zero-width wires)
      0.U(1.W)
    }
  }

  def get_next_state(state: UInt, touch_way: UInt): UInt = {
    val touch_way_sized =
      if (touch_way.getWidth < log2Ceil(n_ways)) ZeroExt(touch_way, log2Ceil(n_ways))
      else touch_way(log2Ceil(n_ways) - 1, 0)
    get_next_state(state, touch_way_sized, n_ways)
  }

  /** @param state state_reg bits for this sub-tree
   * @param tree_nways number of ways in this sub-tree
   */
  def get_replace_way(state: UInt, tree_nways: Int): UInt = {
    require(state.getWidth == (tree_nways - 1), s"wrong state bits width ${state.getWidth} for $tree_nways ways")

    // this algorithm recursively descends the binary tree, filling in the way-to-replace encoded value from msb to lsb
    if (tree_nways > 2) {
      // we are at a branching node in the tree, so recurse
      val right_nways: Int = 1 << (log2Ceil(tree_nways) - 1) // number of ways in the right sub-tree
      val left_nways: Int = tree_nways - right_nways // number of ways in the left sub-tree
      val left_subtree_older = state(tree_nways - 2)
      val left_subtree_state = state(tree_nways - 3, right_nways - 1)
      val right_subtree_state = state(right_nways - 2, 0)

      if (left_nways > 1) {
        // we are at a branching node in the tree with both left and right sub-trees, so recurse both sub-trees
        Cat(
          left_subtree_older, // return the top state bit (current tree node) as msb of the way-to-replace encoded value
          Mux(
            left_subtree_older, // if left sub-tree is older, recurse left, else recurse right
            get_replace_way(left_subtree_state, left_nways), // recurse left
            get_replace_way(right_subtree_state, right_nways),
          ),
        ) // recurse right
      } else {
        // we are at a branching node in the tree with only a right sub-tree, so recurse only right sub-tree
        Cat(
          left_subtree_older, // return the top state bit (current tree node) as msb of the way-to-replace encoded value
          Mux(
            left_subtree_older, // if left sub-tree is older, return and do not recurse right
            0.U(1.W),
            get_replace_way(right_subtree_state, right_nways),
          ),
        ) // recurse right
      }
    } else if (tree_nways == 2) {
      // we are at a leaf node at the end of the tree, so just return the single state bit as lsb of the way-to-replace encoded value
      state(0)
    } else { // tree_nways <= 1
      // we are at an empty node in an unbalanced tree for non-power-of-2 ways, so return single zero bit as lsb of the way-to-replace encoded value
      0.U(1.W)
    }
  }

  def get_replace_way(state: UInt): UInt = get_replace_way(state, n_ways)

  /** @param state state_reg bits for this sub-tree
   * @param tree_nways number of ways in this sub-tree
   */
  def get_replace_way(state: UInt, tree_nways: Int, cond: UInt): (Bool, UInt) = {
    require(state.getWidth == (tree_nways - 1), s"wrong state bits width ${state.getWidth} for $tree_nways ways")
    require(cond.getWidth == tree_nways, s"wrong cond bits width ${cond.getWidth} for $tree_nways ways")
    val tree_cond = cond.asUInt.orR

    // this algorithm recursively descends the binary tree, filling in the way-to-replace encoded value from msb to lsb
    if (tree_nways > 2) {
      // we are at a branching node in the tree, so recurse
      val right_nways: Int = 1 << (log2Ceil(tree_nways) - 1) // number of ways in the right sub-tree
      val left_nways: Int = tree_nways - right_nways // number of ways in the left sub-tree
      val left_subtree_older = state(tree_nways - 2)
      val left_subtree_state = state(tree_nways - 3, right_nways - 1)
      val right_subtree_state = state(right_nways - 2, 0)
      val left_subtree_cond = cond(tree_nways - 1, right_nways)
      val right_subtree_cond = cond(right_nways - 1, 0)

      // recurse left
      val (left_valid, left_way) = if (left_nways > 1) {
        // we are at a branching node in the tree with both left and right sub-trees, so recurse both sub-trees
        get_replace_way(left_subtree_state, left_nways, left_subtree_cond)
      } else {
        // we are at a branching node in the tree with only a right sub-tree, so recurse only right sub-tree
        require(left_subtree_cond.getWidth == 1)
        (left_subtree_cond(0), 0.U(1.W))
      }

      // recurse right
      val (right_valid, right_way) = get_replace_way(right_subtree_state, right_nways, right_subtree_cond)

      // | left_older, left_valid, right_valid | result
      // |    true   ,    true   ,    true     | true
      // |    true   ,    true   ,    false    | true
      // |    true   ,    false  ,    true     | false
      // |    true   ,    false  ,    false    | true
      // |    false  ,    true   ,    true     | false
      // |    false  ,    true   ,    false    | true
      // |    false  ,    false  ,    true     | false
      // |    false  ,    false  ,    false    | false
      val sel_left_subtree = Mux(left_subtree_older, left_valid || !right_valid, left_valid && !right_valid)
      val replace_way = Cat(
        sel_left_subtree, // return the top state bit (current tree node) as msb of the way-to-replace encoded value
        Mux(
          sel_left_subtree, // if left sub-tree is older, recurse left, else recurse right
          left_way,
          right_way,
        ),
      )
      (tree_cond, replace_way)
    } else if (tree_nways == 2) {
      // we are at a leaf node at the end of the tree, so just return the single state bit as lsb of the way-to-replace encoded value
      require(cond.getWidth == 2)
      (tree_cond, Mux(state(0), cond(1) || !cond(0), cond(1) && !cond(0)))
    } else { // tree_nways <= 1
      // we are at an empty node in an unbalanced tree for non-power-of-2 ways, so return single zero bit as lsb of the way-to-replace encoded value
      (tree_cond, 0.U(1.W))
    }
  }

  def get_replace_way(state: UInt, cond: UInt): UInt = get_replace_way(state, n_ways, cond)._2

  def way = get_replace_way(state_reg)
  def miss = access(way)
  def hit = {}
}
