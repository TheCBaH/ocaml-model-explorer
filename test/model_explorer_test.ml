open Model_explorer
open Jsont_bytesrw

let%expect_test "keyValue" =
  let json = Result.get_ok @@ encode_string KeyValue.jsont @@ KeyValue.create ~key:"boo" ~value:"bar" in
  print_string json;
  [%expect {| {"key":"boo","value":"bar"} |}];
  ()

let%expect_test "incomingEdge without metadata" =
  let edge = IncomingEdge.create ~sourceNodeId:"nodeA" ~sourceNodeOutputId:"out1" ~targetNodeInputId:"in2" () in
  let json = Result.get_ok @@ encode_string IncomingEdge.jsont edge in
  print_string json;
  [%expect {| {"sourceNodeId":"nodeA","sourceNodeOutputId":"out1","targetNodeInputId":"in2"} |}];
  ()

let%expect_test "incomingEdge with metadata" =
  let metadata = String_map.(empty |> add "meta1" "value1" |> add "meta2" "value2") in
  let edge =
    IncomingEdge.create ~sourceNodeId:"nodeB" ~sourceNodeOutputId:"out3" ~targetNodeInputId:"in4" ~metadata ()
  in
  let json = Result.get_ok @@ encode_string ~format:Jsont.Indent IncomingEdge.jsont edge in
  print_string json;
  [%expect
    {|
      {
        "sourceNodeId": "nodeB",
        "sourceNodeOutputId": "out3",
        "targetNodeInputId": "in4",
        "metadata": {
          "meta1": "value1",
          "meta2": "value2"
        }
      } |}];
  ()

let%expect_test "metadataItem" =
  let attrs = [ KeyValue.create ~key:"attr1" ~value:"val1"; KeyValue.create ~key:"attr2" ~value:"val2" ] in
  let item = MetadataItem.create ~id:"item1" ~attrs in
  let json = Result.get_ok @@ encode_string ~format:Jsont.Indent MetadataItem.jsont item in
  print_string json;
  [%expect
    {|
    {
      "id": "item1",
      "attrs": [
        {
          "key": "attr1",
          "value": "val1"
        },
        {
          "key": "attr2",
          "value": "val2"
        }
      ]
    } |}];
  ()

let%expect_test "outgoingEdge without metadata" =
  let edge = OutgoingEdge.create ~targetNodeId:"nodeC" ~sourceNodeOutputId:"out5" ~targetNodeInputId:"in6" () in
  let json = Result.get_ok @@ encode_string OutgoingEdge.jsont edge in
  print_string json;
  [%expect {| {"targetNodeId":"nodeC","sourceNodeOutputId":"out5","targetNodeInputId":"in6"} |}];
  ()

let%expect_test "outgoingEdge with metadata" =
  let metadata = String_map.(empty |> add "meta3" "value3" |> add "meta4" "value4") in
  let edge =
    OutgoingEdge.create ~targetNodeId:"nodeD" ~sourceNodeOutputId:"out7" ~targetNodeInputId:"in8" ~metadata ()
  in
  let json = Result.get_ok @@ encode_string ~format:Jsont.Indent OutgoingEdge.jsont edge in
  print_string json;
  [%expect
    {|
      {
        "targetNodeId": "nodeD",
        "sourceNodeOutputId": "out7",
        "targetNodeInputId": "in8",
        "metadata": {
          "meta3": "value3",
          "meta4": "value4"
        }
      } |}];
  ()

let%expect_test "graphNodeStyle empty" =
  let style = GraphNodeStyle.create () in
  let json = Result.get_ok @@ encode_string GraphNodeStyle.jsont style in
  print_string json;
  [%expect {| {} |}];
  ()

let%expect_test "graphNodeStyle some fields" =
  let style = GraphNodeStyle.create ~backgroundColor:"#ff0000" ~hoveredBorderColor:"#00ff00" () in
  let json = Result.get_ok @@ encode_string GraphNodeStyle.jsont style in
  print_string json;
  [%expect {| {"backgroundColor":"#ff0000","hoveredBorderColor":"#00ff00"} |}];
  ()

let%expect_test "graphNodeStyle all fields" =
  let style = GraphNodeStyle.create ~backgroundColor:"red" ~borderColor:"blue" ~hoveredBorderColor:"green" () in
  let json = Result.get_ok @@ encode_string GraphNodeStyle.jsont style in
  print_string json;
  [%expect {| {"backgroundColor":"red","borderColor":"blue","hoveredBorderColor":"green"} |}];
  ()

let%expect_test "graphNodeConfig empty" =
  let config = GraphNodeConfig.create () in
  let json = Result.get_ok @@ encode_string GraphNodeConfig.jsont config in
  print_string json;
  [%expect {| {} |}];
  ()

let%expect_test "graphNodeConfig true" =
  let config = GraphNodeConfig.create ~pinToGroupTop:true () in
  let json = Result.get_ok @@ encode_string GraphNodeConfig.jsont config in
  print_string json;
  [%expect {| {"pinToGroupTop":true} |}];
  ()

let%expect_test "graphNodeConfig false" =
  let config = GraphNodeConfig.create ~pinToGroupTop:false () in
  let json = Result.get_ok @@ encode_string GraphNodeConfig.jsont config in
  print_string json;
  [%expect {| {"pinToGroupTop":false} |}];
  ()

let%expect_test "graphNode minimal" =
  let node = GraphNode.create ~id:"node1" ~label:"Node One" ~namespace:"a/b" () in
  let json = Result.get_ok @@ encode_string GraphNode.jsont node in
  print_string json;
  [%expect {| {"id":"node1","label":"Node One","namespace":"a/b"} |}];
  ()

let%expect_test "graphNode with optional fields" =
  let attrs = [ KeyValue.create ~key:"attr1" ~value:"val1" ] in
  let incomingEdges =
    [ IncomingEdge.create ~sourceNodeId:"prevNode" ~sourceNodeOutputId:"out0" ~targetNodeInputId:"in0" () ]
  in
  let inputsMetadata = [ MetadataItem.create ~id:"inputMeta1" ~attrs:[] ] in
  let outputsMetadata =
    [ MetadataItem.create ~id:"outputMeta1" ~attrs:[ KeyValue.create ~key:"shape" ~value:"[1, 10]" ] ]
  in
  let style = GraphNodeStyle.create ~backgroundColor:"yellow" () in
  let config = GraphNodeConfig.create ~pinToGroupTop:true () in
  let node =
    GraphNode.create ~id:"node2" ~label:"Node Two" ~namespace:"a/c" ~subgraphIds:[ "subgraph1"; "subgraph2" ] ~attrs
      ~incomingEdges ~inputsMetadata ~outputsMetadata ~style ~config ()
  in
  let json = Result.get_ok @@ encode_string ~format:Jsont.Indent GraphNode.jsont node in
  print_string json;
  [%expect
    {|
      {
        "id": "node2",
        "label": "Node Two",
        "namespace": "a/c",
        "subgraphIds": [
          "subgraph1",
          "subgraph2"
        ],
        "attrs": [
          {
            "key": "attr1",
            "value": "val1"
          }
        ],
        "incomingEdges": [
          {
            "sourceNodeId": "prevNode",
            "sourceNodeOutputId": "out0",
            "targetNodeInputId": "in0"
          }
        ],
        "inputsMetadata": [
          {
            "id": "inputMeta1",
            "attrs": []
          }
        ],
        "outputsMetadata": [
          {
            "id": "outputMeta1",
            "attrs": [
              {
                "key": "shape",
                "value": "[1, 10]"
              }
            ]
          }
        ],
        "style": {
          "backgroundColor": "yellow"
        },
        "config": {
          "pinToGroupTop": true
        }
      } |}];
  ()

let%expect_test "groupNodeAttributes" =
  let ns1_attrs = String_map.(empty |> add "attr1" "val1") in
  let ns2_attrs = String_map.(empty |> add "attr2" "val2" |> add "attr3" "val3") in
  let attrs = String_map.(empty |> add "ns1" ns1_attrs |> add "ns2" ns2_attrs) in
  let json = Result.get_ok @@ encode_string ~format:Jsont.Indent GroupNodeAttributes.jsont attrs in
  print_string json;
  [%expect
    {|
    {
      "ns1": {
        "attr1": "val1"
      },
      "ns2": {
        "attr2": "val2",
        "attr3": "val3"
      }
    } |}];
  ()

let%expect_test "graph minimal" =
  let nodes = [ GraphNode.create ~id:"node1" ~label:"Node One" ~namespace:"a/b" () ] in
  let graph = Graph.create ~id:"graph1" ~nodes () in
  let json = Result.get_ok @@ encode_string ~format:Jsont.Indent Graph.jsont graph in
  print_string json;
  [%expect
    {|
    {
      "id": "graph1",
      "nodes": [
        {
          "id": "node1",
          "label": "Node One",
          "namespace": "a/b"
        }
      ]
    } |}];
  ()

let%expect_test "graph with optional fields" =
  let nodes =
    [
      GraphNode.create ~id:"node1" ~label:"Node One" ~namespace:"a/b" ();
      GraphNode.create ~id:"node2" ~label:"Node Two" ~namespace:"a/c" ~subgraphIds:[ "subgraph1" ] ();
    ]
  in
  let groupNodeAttributes = String_map.(empty |> add "a" String_map.(empty |> add "groupAttr" "groupVal")) in
  let graph =
    Graph.create ~id:"graph2" ~nodes ~collectionLabel:"My Collection" ~groupNodeAttributes ~subGraphIds:[ "subgraph1" ]
      ~parentGraphIds:[ "parentGraph" ] ()
  in
  let json = Result.get_ok @@ encode_string ~format:Jsont.Indent Graph.jsont graph in
  print_string json;
  [%expect
    {|
    {
      "id": "graph2",
      "collectionLabel": "My Collection",
      "nodes": [
        {
          "id": "node1",
          "label": "Node One",
          "namespace": "a/b"
        },
        {
          "id": "node2",
          "label": "Node Two",
          "namespace": "a/c",
          "subgraphIds": [
            "subgraph1"
          ]
        }
      ],
      "groupNodeAttributes": {
        "a": {
          "groupAttr": "groupVal"
        }
      },
      "subGraphIds": [
        "subgraph1"
      ],
      "parentGraphIds": [
        "parentGraph"
      ]
    } |}];
  ()

let%expect_test "graphCollection minimal" =
  let nodes = [ GraphNode.create ~id:"node1" ~label:"Node One" ~namespace:"a/b" () ] in
  let graph = Graph.create ~id:"graph1" ~nodes () in
  let collection = GraphCollection.create ~label:"My Collection" ~graphs:[ graph ] in
  let json = Result.get_ok @@ encode_string ~format:Jsont.Indent GraphCollection.jsont collection in
  print_string json;
  [%expect
    {|
    {
      "label": "My Collection",
      "graphs": [
        {
          "id": "graph1",
          "nodes": [
            {
              "id": "node1",
              "label": "Node One",
              "namespace": "a/b"
            }
          ]
        }
      ]
    } |}];
  ()

let%expect_test "graphCollection with optional fields" =
  let nodes1 = [ GraphNode.create ~id:"node1" ~label:"Node One" ~namespace:"a/b" () ] in
  let graph1 = Graph.create ~id:"graph1" ~nodes:nodes1 () in
  let nodes2 = [ GraphNode.create ~id:"node2" ~label:"Node Two" ~namespace:"c" () ] in
  let graph2 = Graph.create ~id:"graph2" ~nodes:nodes2 () in
  let collection = GraphCollection.create ~label:"My Collection 2" ~graphs:[ graph1; graph2 ] in
  let json = Result.get_ok @@ encode_string ~format:Jsont.Indent GraphCollection.jsont collection in
  print_string json;
  [%expect
    {|
    {
      "label": "My Collection 2",
      "graphs": [
        {
          "id": "graph1",
          "nodes": [
            {
              "id": "node1",
              "label": "Node One",
              "namespace": "a/b"
            }
          ]
        },
        {
          "id": "graph2",
          "nodes": [
            {
              "id": "node2",
              "label": "Node Two",
              "namespace": "c"
            }
          ]
        }
      ]
    } |}];
  ()
