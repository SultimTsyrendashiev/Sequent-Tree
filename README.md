# Sequent-Tree

type Sequent -- represents sequent
data SeqTree -- represents sequent tree

Checks sequent's validness, writes one counter example if exists, creates .dot file, which can be converted to image:
# isValid :: Sequent -> IO ()

Creates sequent tree:
# createTree :: Sequent -> SeqTree
