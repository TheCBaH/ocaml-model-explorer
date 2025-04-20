(* based on src/ui/src/components/visualizer/common/types.ts *)
(** An object with "key" and "value" field. *)
module KeyValue = struct
  type t = { key : string;  (** The key. *) value : string  (** The value. *) }

  let make key value = { key; value }

  (** [create ~key ~value] creates a new key-value pair.
      @param key The key.
      @param value The value.
      @return A new key-value pair. *)
  let create ~key ~value = make key value

  let key t = t.key
  let value t = t.value

  let jsont =
    Jsont.Object.map ~kind:"KeyValue" make
    |> Jsont.Object.mem "key" Jsont.string ~enc:key
    |> Jsont.Object.mem "value" Jsont.string ~enc:value
    |> Jsont.Object.finish
end

(** A type for a list of key-value pairs. *)
module KeyValueList = struct
  type t = KeyValue.t list

  let jsont = Jsont.list KeyValue.jsont
end

(* based on src/ui/src/components/visualizer/common/types.ts *)

(** An item in input/output metadata. *)
module MetadataItem = struct
  type t = {
    id : string;  (** The id of the metadata item. *)
    attrs : KeyValueList.t;  (** Attributes associated with the metadata item. *)
  }

  let make id attrs = { id; attrs }

  (** [create ~id ~attrs] creates a new metadata item.
      @param id The id of the metadata item.
      @param attrs Attributes associated with the metadata item.
      @return A new metadata item. *)
  let create ~id ~attrs = make id attrs

  let id t = t.id
  let attrs t = t.attrs

  let jsont =
    Jsont.Object.map ~kind:"MetadataItem" make
    |> Jsont.Object.mem "id" Jsont.string ~enc:id
    |> Jsont.Object.mem "attrs" KeyValueList.jsont ~enc:attrs
    |> Jsont.Object.finish
end

module String_map = Map.Make (String)

(** A type for key-value pairs represented as a string map. *)
module KeyValuePairs = struct
  type t = string String_map.t

  let jsont = Jsont.Object.as_string_map ~kind:"KeyValuePairs" Jsont.string
end

(** An incoming edge in the graph. *)
module IncomingEdge = struct
  type t = {
    sourceNodeId : string;  (** The id of the source node (where the edge comes from). *)
    sourceNodeOutputId : string;  (** The id of the output from the source node that this edge goes out of. *)
    targetNodeInputId : string;  (** The id of the input from the target node (this node) that this edge connects to. *)
    metadata : KeyValuePairs.t option;  (** Other associated metadata for this edge. *)
  }

  let make sourceNodeId sourceNodeOutputId targetNodeInputId metadata =
    { sourceNodeId; sourceNodeOutputId; targetNodeInputId; metadata }

  (** [create ~sourceNodeId ~sourceNodeOutputId ~targetNodeInputId ?metadata ()] creates a new incoming edge.
      @param sourceNodeId The id of the source node (where the edge comes from).
      @param sourceNodeOutputId The id of the output from the source node that this edge goes out of.
      @param targetNodeInputId The id of the input from the target node (this node) that this edge connects to.
      @param metadata Other associated metadata for this edge.
      @return A new incoming edge. *)
  let create ~sourceNodeId ~sourceNodeOutputId ~targetNodeInputId ?metadata () =
    make sourceNodeId sourceNodeOutputId targetNodeInputId metadata

  let sourceNodeId t = t.sourceNodeId
  let sourceNodeOutputId t = t.sourceNodeOutputId
  let targetNodeInputId t = t.targetNodeInputId
  let metadata t = t.metadata

  let jsont =
    Jsont.Object.map ~kind:"IncomingEdge" (fun sourceNodeId sourceNodeOutputId targetNodeInputId metadata ->
        { sourceNodeId; sourceNodeOutputId; targetNodeInputId; metadata })
    |> Jsont.Object.mem "sourceNodeId" Jsont.string ~enc:sourceNodeId
    |> Jsont.Object.mem "sourceNodeOutputId" Jsont.string ~enc:sourceNodeOutputId
    |> Jsont.Object.mem "targetNodeInputId" Jsont.string ~enc:targetNodeInputId
    |> Jsont.Object.opt_mem "metadata" KeyValuePairs.jsont ~enc:metadata
    |> Jsont.Object.finish
end

(** The style of the op node. *)
module GraphNodeStyle = struct
  type t = {
    backgroundColor : string option;  (** The background color of the node. It should be in css format. *)
    borderColor : string option;  (** The border color of the node. It should be in css format. *)
    hoveredBorderColor : string option;
        (** The border color of the node when it is hovered. It should be in css format. *)
  }

  let make backgroundColor borderColor hoveredBorderColor = { backgroundColor; borderColor; hoveredBorderColor }

  (** [create ?backgroundColor ?borderColor ?hoveredBorderColor ()] creates a new graph node style.
      @param backgroundColor The background color of the node. It should be in css format.
      @param borderColor The border color of the node. It should be in css format.
      @param hoveredBorderColor The border color of the node when it is hovered. It should be in css format.
      @return A new graph node style. *)
  let create ?backgroundColor ?borderColor ?hoveredBorderColor () = make backgroundColor borderColor hoveredBorderColor

  let backgroundColor t = t.backgroundColor
  let borderColor t = t.borderColor
  let hoveredBorderColor t = t.hoveredBorderColor

  let jsont =
    Jsont.Object.map ~kind:"GraphNodeStyle" make
    |> Jsont.Object.opt_mem "backgroundColor" Jsont.string ~enc:backgroundColor
    |> Jsont.Object.opt_mem "borderColor" Jsont.string ~enc:borderColor
    |> Jsont.Object.opt_mem "hoveredBorderColor" Jsont.string ~enc:hoveredBorderColor
    |> Jsont.Object.finish
end

(** Custom configs for a graph node. *)
module GraphNodeConfig = struct
  type t = { pinToGroupTop : bool option  (** Whether to pin the node to the top of the group it belongs to. *) }

  let make pinToGroupTop = { pinToGroupTop }

  (** [create ?pinToGroupTop ()] creates a new graph node config.
      @param pinToGroupTop Whether to pin the node to the top of the group it belongs to.
      @return A new graph node config. *)
  let create ?pinToGroupTop () = make pinToGroupTop

  let pinToGroupTop t = t.pinToGroupTop

  let jsont =
    Jsont.Object.map ~kind:"GraphNodeConfig" make
    |> Jsont.Object.opt_mem "pinToGroupTop" Jsont.bool ~enc:pinToGroupTop
    |> Jsont.Object.finish
end

(** A single node in the graph. *)
module GraphNode = struct
  type t = {
    id : string;  (** The unique id of the node. *)
    label : string;  (** The label of the node, displayed on the node in the model graph. *)
    namespace : string;
        (** The namespace/hierarchy data of the node in the form of a "path" (e.g. * a/b/c). Don't include the node
            label as the last component of the * namespace. The visualizer will use this data to visualize nodes in a
            nested * way. *)
    subgraphIds : string list option;
        (** Ids of subgraphs that this node goes into. The graphs referenced here should be the ones from the `graphs`
            field in `GraphList`. Once set, users will be able to click this node, pick a subgraph from a drop-down
            list, and see the visualization for the selected subgraph. *)
    attrs : KeyValueList.t option;  (** The attributes of the node. *)
    incomingEdges : IncomingEdge.t list option;  (** A list of incoming edges. *)
    inputsMetadata : MetadataItem.t list option;  (** Metadata for inputs. *)
    outputsMetadata : MetadataItem.t list option;  (** Metadata for outputs. *)
    style : GraphNodeStyle.t option;  (** The default style of the node. *)
    config : GraphNodeConfig.t option;  (** Custom configs for the node. *)
  }

  let make id label namespace subgraphIds attrs incomingEdges inputsMetadata outputsMetadata style config =
    { id; label; namespace; subgraphIds; attrs; incomingEdges; inputsMetadata; outputsMetadata; style; config }

  (** [create ~id ~label ~namespace ?subgraphIds ?attrs ?incomingEdges ?inputsMetadata ?outputsMetadata ?style ?config
       ()] creates a new graph node.
      @param id The unique id of the node.
      @param label The label of the node, displayed on the node in the model graph.
      @param namespace
        The namespace/hierarchy data of the node in the form of a "path" (e.g. * a/b/c). Don't include the node label as
        the last component of the * namespace. The visualizer will use this data to visualize nodes in a nested * way.
      @param subgraphIds
        Ids of subgraphs that this node goes into. The graphs referenced here should be the ones from the `graphs` field
        in `GraphList`. Once set, users will be able to click this node, pick a subgraph from a drop-down list, and see
        the visualization for the selected subgraph.
      @param attrs The attributes of the node.
      @param incomingEdges A list of incoming edges.
      @param inputsMetadata Metadata for inputs.
      @param outputsMetadata Metadata for outputs.
      @param style The default style of the node.
      @param config Custom configs for the node.
      @return A new graph node. *)
  let create ~id ~label ~namespace ?subgraphIds ?attrs ?incomingEdges ?inputsMetadata ?outputsMetadata ?style ?config ()
      =
    make id label namespace subgraphIds attrs incomingEdges inputsMetadata outputsMetadata style config

  let id t = t.id
  let label t = t.label
  let namespace t = t.namespace
  let subgraphIds t = t.subgraphIds
  let attrs t = t.attrs
  let incomingEdges t = t.incomingEdges
  let inputsMetadata t = t.inputsMetadata
  let outputsMetadata t = t.outputsMetadata
  let style t = t.style
  let config t = t.config

  let jsont =
    Jsont.Object.map ~kind:"GraphNode" make
    |> Jsont.Object.mem "id" Jsont.string ~enc:id
    |> Jsont.Object.mem "label" Jsont.string ~enc:label
    |> Jsont.Object.mem "namespace" Jsont.string ~enc:namespace
    |> Jsont.Object.opt_mem "subgraphIds" (Jsont.list Jsont.string) ~enc:subgraphIds
    |> Jsont.Object.opt_mem "attrs" KeyValueList.jsont ~enc:attrs
    |> Jsont.Object.opt_mem "incomingEdges" (Jsont.list IncomingEdge.jsont) ~enc:incomingEdges
    |> Jsont.Object.opt_mem "inputsMetadata" (Jsont.list MetadataItem.jsont) ~enc:inputsMetadata
    |> Jsont.Object.opt_mem "outputsMetadata" (Jsont.list MetadataItem.jsont) ~enc:outputsMetadata
    |> Jsont.Object.opt_mem "style" GraphNodeStyle.jsont ~enc:style
    |> Jsont.Object.opt_mem "config" GraphNodeConfig.jsont ~enc:config
    |> Jsont.Object.finish
end

(** An outgoing edge in the graph. *)
module OutgoingEdge = struct
  type t = {
    targetNodeId : string;  (** The id of the target node (where the edge connects to). *)
    sourceNodeOutputId : string;  (** The id of the output from the source node that this edge goes out of. *)
    targetNodeInputId : string;
        (** The id of the input from the target node (this node) that this edge * connects to. *)
    metadata : KeyValuePairs.t option;  (** Other associated metadata for this edge. *)
  }

  let make targetNodeId sourceNodeOutputId targetNodeInputId metadata =
    { targetNodeId; sourceNodeOutputId; targetNodeInputId; metadata }

  (** [create ~targetNodeId ~sourceNodeOutputId ~targetNodeInputId ?metadata ()] creates a new outgoing edge.
      @param targetNodeId The id of the target node (where the edge connects to).
      @param sourceNodeOutputId The id of the output from the source node that this edge goes out of.
      @param targetNodeInputId The id of the input from the target node (this node) that this edge * connects to.
      @param metadata Other associated metadata for this edge.
      @return A new outgoing edge. *)

  let create ~targetNodeId ~sourceNodeOutputId ~targetNodeInputId ?metadata () =
    make targetNodeId sourceNodeOutputId targetNodeInputId metadata

  let targetNodeId t = t.targetNodeId
  let sourceNodeOutputId t = t.sourceNodeOutputId
  let targetNodeInputId t = t.targetNodeInputId
  let metadata t = t.metadata

  let jsont =
    Jsont.Object.map ~kind:"OutgoingEdge" (fun targetNodeId sourceNodeOutputId targetNodeInputId metadata ->
        { targetNodeId; sourceNodeOutputId; targetNodeInputId; metadata })
    |> Jsont.Object.mem "targetNodeId" Jsont.string ~enc:targetNodeId
    |> Jsont.Object.mem "sourceNodeOutputId" Jsont.string ~enc:sourceNodeOutputId
    |> Jsont.Object.mem "targetNodeInputId" Jsont.string ~enc:targetNodeInputId
    |> Jsont.Object.opt_mem "metadata" KeyValuePairs.jsont ~enc:metadata
    |> Jsont.Object.finish
end

(** A single attribute item for group node. *)
module GroupNodeAttributeItem = struct
  type t = string

  let jsont = Jsont.string
end

(** Attributes for group nodes. *)
module GroupNodeAttributes = struct
  type t = string String_map.t String_map.t
  (** From group's namespace to its attributes (key-value pairs). Use empty group namespace for the model-level
      attributes (i.e. shown in side panel when no node is selected). *)

  let jsont =
    Jsont.Object.as_string_map ~kind:"GroupNodeAttributes"
      (Jsont.Object.as_string_map ~kind:"GroupNamespaceAttributes" Jsont.string)
end

(** A graph to be visualized. Clients need to convert their own graphs into this format and pass it into the visualizer
    through `GraphCollection`. The visualizer will then process the graph and convert it into its internal * format
    before visualizing it. *)
module Graph = struct
  type t = {
    id : string;  (** The id of the graph. *)
    collectionLabel : string option;
        (** The label of the collection this graph belongs to. This field will be set internally. *)
    nodes : GraphNode.t list;  (** A list of nodes in the graph. *)
    groupNodeAttributes : GroupNodeAttributes.t option;
        (** Attributes for group nodes. Displayed in the side panel when the group is selected. *)
    (* The following fields are set by model explorer internally. *)
    subGraphIds : string list option;  (** The ids of all its subgraphs. *)
    parentGraphIds : string list option;  (** The ids of its parent graphs. *)
  }

  let make id collectionLabel nodes groupNodeAttributes subGraphIds parentGraphIds =
    { id; collectionLabel; nodes; groupNodeAttributes; subGraphIds; parentGraphIds }

  let create ~id ~nodes ?collectionLabel ?groupNodeAttributes ?subGraphIds ?parentGraphIds () =
    make id collectionLabel nodes groupNodeAttributes subGraphIds parentGraphIds

  (** [create ~id ~nodes ?collectionLabel ?groupNodeAttributes ?subGraphIds ?parentGraphIds ()] creates a new graph.
      @param id The id of the graph.
      @param nodes A list of nodes in the graph.
      @param collectionLabel The label of the collection this graph belongs to. This field will be set internally.
      @param groupNodeAttributes Attributes for group nodes. Displayed in the side panel when the group is selected.
      @param subGraphIds The ids of all its subgraphs.
      @param parentGraphIds The ids of its parent graphs.
      @return A new graph.

      The following fields are set by model explorer internally.
      - subGraphIds : The ids of all its subgraphs.
      - parentGraphIds : The ids of its parent graphs. *)

  let id t = t.id
  let collectionLabel t = t.collectionLabel
  let nodes t = t.nodes
  let groupNodeAttributes t = t.groupNodeAttributes
  let subGraphIds t = t.subGraphIds
  let parentGraphIds t = t.parentGraphIds

  let jsont =
    Jsont.Object.map ~kind:"Graph" make
    |> Jsont.Object.mem "id" Jsont.string ~enc:id
    |> Jsont.Object.opt_mem "collectionLabel" Jsont.string ~enc:collectionLabel
    |> Jsont.Object.mem "nodes" (Jsont.list GraphNode.jsont) ~enc:nodes
    |> Jsont.Object.opt_mem "groupNodeAttributes" GroupNodeAttributes.jsont ~enc:groupNodeAttributes
    |> Jsont.Object.opt_mem "subGraphIds" (Jsont.list Jsont.string) ~enc:subGraphIds
    |> Jsont.Object.opt_mem "parentGraphIds" (Jsont.list Jsont.string) ~enc:parentGraphIds
    |> Jsont.Object.finish
end

(** A collection of graphs. This is the input to the visualizer. *)
module GraphCollection = struct
  type t = {
    label : string;  (** The label of the collection. *)
    graphs : Graph.t list;  (** The graphs inside the collection. *)
  }

  let make label graphs = { label; graphs }

  (** [create ~label ~graphs] creates a new graph collection.
      @param label The label of the collection.
      @param graphs The graphs inside the collection.
      @return A new graph collection. *)
  let create ~label ~graphs = make label graphs

  let label t = t.label
  let graphs t = t.graphs

  let jsont =
    Jsont.Object.map ~kind:"GraphCollection" make
    |> Jsont.Object.mem "label" Jsont.string ~enc:label
    |> Jsont.Object.mem "graphs" (Jsont.list Graph.jsont) ~enc:graphs
    |> Jsont.Object.finish
end
