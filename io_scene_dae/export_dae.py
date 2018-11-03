# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####
from bpy_types import Node
from platform import node
from mesh_inset.offset import Offset
import os
import time
import math  # math.pi
import shutil
import bpy
import bmesh
from mathutils import Vector, Matrix, Euler, Color
import itertools
from statistics import mean
import sys
from urllib.parse import urlparse

#
# Author: Gregery Barton
# Contact: gregery20@yahoo.com.au
#
# Originally authored by:
# Script copyright (C) Juan Linietsky
# Contact Info: juan@codenix.com
#
#

"""
This script is an exporter to the Khronos Collada file format.

http://www.khronos.org/collada/
"""

# according to collada spec, order matters
S_ASSET = 0
S_IMGS = 1
S_FX = 2
S_MATS = 3
S_GEOM = 4
S_MORPH = 5
S_SKIN = 6
S_CONT = 7
S_CAMS = 8
S_LAMPS = 9
S_LIBRARY_NODES = 10
S_NODES = 11
S_P_MATS = 12
S_P_MODEL = 13
S_P_SCENE = 14
S_ANIM = 15
S_ANIM_CLIPS = 16

CMP_EPSILON = 0.001

IMAGE_PATH = "daeimages"

# number of times to recalculate frame to try and settle IK constraints into final
# position

SETTLE_IK_ITERATIONS = 25

def matrix_equal(a, b):
 for aa, bb in zip([e for v in a for e in v], [e for v in b for e in v]):
  if (abs(aa - bb) > CMP_EPSILON):
   return False
 return True

def vector_equal(a, b):
 for aa, bb in zip([e for e in a], [e for e in b]):
  if (abs(aa - bb) > CMP_EPSILON):
   return False
 return True

def numarr_alpha(a, mult=1.0):
 s = " ".join([strflt(x * mult) for x in a])
 if len(a) == 3:
  s += " 1.0"
 return s

def strarr(arr):
 return " ".join([strflt(e) for e in arr])

def strflt(x):
 return '{0:.4f}'.format(x)

class DaeExporter:

 def strmtx(self, mtx):
  if self.axis_type == "ZUP":
   out = mtx
  elif self.axis_type == "YUPR":
   out = Matrix([mtx[0], mtx[2], -mtx[1], mtx[3]])
   out.transpose()
   out = Matrix([out[0], out[2], -out[1], out[3]])
   out.transpose()
  elif self.axis_type == "YUPL":
   out = Matrix([mtx[0], mtx[2], mtx[1], mtx[3]])
   out.transpose()
   out = Matrix([out[0], out[2], out[1], out[3]])
   out.transpose()
  return " ".join([strflt(e) for v in out for e in v])

 def strxyz(self, xyz, abso=False):
  if self.axis_type == "ZUP":
   out = xyz
  elif self.axis_type == "YUPR":
   out = [xyz[0], xyz[2], -xyz[1]]
  elif self.axis_type == "YUPL":
   out = [xyz[0], xyz[2], xyz[1]]
  if abso:
   out = [abs(c) for c in out]

  return " ".join([strflt(e) for e in out])

 def new_id(self, t):
  self.last_id += 1
  return "id-" + t + "-" + str(self.last_id)

 def quote_spaces(self, name):
  if (name.find(" ") != -1):
   return '"' + name + '"'
  else:
   return name

 def writel(self, section, indent, text):
  if (not (section in self.sections)):
   self.sections[section] = []
  line = ""
  for x in range(indent):
   line += " "
  line += text
  self.sections[section].append(line)

 def clean_path(self, pathname):
  p = pathname.replace("\\", "/")
  p = p.replace("\\\\", "/")
  return p

 def export_image(self, image, image_id):

  backup_image_path = image.filepath
  imgpath = image.filepath
  if not len(imgpath):
   imgpath = "//" + image_id + ".png"

  save_path = os.path.abspath(os.path.join(os.path.dirname(
   self.path), IMAGE_PATH, urlparse(imgpath).netloc))

  # all images must be saved to a sub folder of the target folder.
  if os.path.commonpath([self.path, save_path]) != os.path.dirname(self.path):
   save_path = os.path.abspath(os.path.join(os.path.dirname(
    self.path), IMAGE_PATH, os.path.basename(save_path)))

  xml_path = os.path.join(
   "./", os.path.relpath(save_path, os.path.dirname(self.path))).replace("\\", "/")

  if (self.config["use_copy_images"] and save_path not in self.image_cache):
   basedir = os.path.dirname(save_path)
   if (not os.path.isdir(basedir)):
    os.makedirs(basedir)

   if imgpath.lower().endswith(tuple(bpy.path.extensions_image)):
    image.filepath = save_path
   else:
    image.filepath = os.path.join(
     basedir, "{}.png".format(image.name))

   try:
    image.save()
   except:
    pass
   image.filepath = backup_image_path
   self.image_cache |= {save_path}

  self.writel(S_IMGS, 1, '<image id="' + image_id +
     '" name="' + image.name + '">')

  # the file path should be surrounded by <ref> tags

  self.writel(S_IMGS, 2, '<init_from><ref>' +
     xml_path + '</ref></init_from>')

  self.writel(S_IMGS, 1, '</image>')

 def ref_id(self, id):
  if "#" not in id:
   return "#" + id
  else:
   return id

 def find_cycles_material_diffuse(self, material):
  if material.node_tree:
   return {node.image for node in material.node_tree.nodes if node.type == "TEX_IMAGE" and node.image.channels >= 3}
  else:
   return set()

 def material_slot_textures(self, material):
  return [slot for slot in material.texture_slots if slot and slot.texture and hasattr(slot.texture, "image") and slot.texture.image]

 def find_blender_material_diffuse(self, material):
  return {slot.texture.image for slot in self.material_slot_textures(material) if slot.use_map_color_diffuse}

 def find_blender_material_alpha(self, material):
  return {slot.texture.image for slot in self.material_slot_textures(material) if slot.use_map_alpha}

 def find_blender_material_specular(self, material):
  return {slot.texture.image for slot in self.material_slot_textures(material) if slot.use_map_color_spec}

 def find_blender_material_emission(self, material):
  return {slot.texture.image for slot in self.material_slot_textures(material) if slot.use_map_emit}

 def find_blender_material_normal(self, material):
  return {slot.texture.image for slot in self.material_slot_textures(material) if slot.use_map_normal}

 def find_blender_material_ambient(self, material):
  return {slot.texture.image for slot in self.material_slot_textures(material) if slot.use_map_ambient}

 def export_sampler2d(self, imgid):
  sampler_sid = imgid + "-sampler"
  self.writel(S_FX, 3, '<newparam sid="' + sampler_sid + '">')
  self.writel(S_FX, 4, '<sampler2D>')
  self.writel(
   S_FX, 5, '<instance_image url="{}"/>'.format(self.ref_id(imgid)))

  self.writel(S_FX, 4, '</sampler2D>')
  self.writel(S_FX, 3, '</newparam>')

  return sampler_sid

 def export_effect(self, material, effect_id, image_lookup):

  self.writel(S_FX, 1, '<effect id="' + effect_id +
     '" name="' + material.name + '">')
  self.writel(S_FX, 2, '<profile_COMMON>')

  # Find and fetch the textures and create sources
  diffuse_tex = self.find_blender_material_diffuse(
   material) or self.find_cycles_material_diffuse(material)
  specular_tex = self.find_blender_material_specular(material)
  emission_tex = self.find_blender_material_emission(material)
  normal_tex = self.find_blender_material_normal(material)
  ambient_tex = self.find_blender_material_ambient(material)
  transparent_tex = self.find_blender_material_alpha(material)

  all_images = diffuse_tex | specular_tex | emission_tex | normal_tex | ambient_tex | transparent_tex
  sampler_sids = {image: self.export_sampler2d(
   image_lookup[image]) for image in all_images}

  self.writel(S_FX, 3, '<technique sid="common">')
  shtype = "blinn"
  self.writel(S_FX, 4, '<' + shtype + '>')
  # ambient? from where?

  self.writel(S_FX, 5, '<emission>')
  if (len(emission_tex)):
   self.writel(S_FX, 6, '<texture texture="' +
      sampler_sids[emission_tex.pop()] + '/>')
  else:
   self.writel(S_FX, 6, '<color>' + numarr_alpha(material.diffuse_color,
                material.emit) + ' </color>')  # not totally right but good enough
  self.writel(S_FX, 5, '</emission>')

  self.writel(S_FX, 5, '<ambient>')
  if (len(ambient_tex)):
   self.writel(S_FX, 6, '<texture texture="' +
      sampler_sids[ambient_tex.pop()] + '" texcoord="UVMap"/>')
  else:
   if self.bpy_context_scene.world:
    self.writel(S_FX, 6, '<color>' + numarr_alpha(
     self.bpy_context_scene.world.ambient_color, material.ambient) + ' </color>')
   else:
    self.writel(
     S_FX, 6, '<color>' + numarr_alpha(Color((0, 0, 0)), material.ambient) + ' </color>')

  self.writel(S_FX, 5, '</ambient>')

  self.writel(S_FX, 5, '<diffuse>')
  if (len(diffuse_tex)):
   self.writel(S_FX, 6, '<texture texture="' +
      sampler_sids[diffuse_tex.pop()] + '" texcoord="UVMap"/>')
  else:
   self.writel(S_FX, 6, '<color>' + numarr_alpha(material.diffuse_color,
                material.diffuse_intensity) + '</color>')
  self.writel(S_FX, 5, '</diffuse>')

  self.writel(S_FX, 5, '<specular>')
  if (len(specular_tex)):
   self.writel(S_FX, 6, '<texture texture="' +
      sampler_sids[specular_tex.pop()] + '" texcoord="UVMap"/>')
  else:
   self.writel(S_FX, 6, '<color>' + numarr_alpha(material.specular_color,
                material.specular_intensity) + '</color>')
  self.writel(S_FX, 5, '</specular>')

  self.writel(S_FX, 5, '<shininess>')
  self.writel(S_FX, 6, '<float>' +
     strflt(material.specular_hardness) + '</float>')
  self.writel(S_FX, 5, '</shininess>')

  self.writel(S_FX, 5, '<reflective>')
  if material.raytrace_mirror.use:
   mirror_factor = material.raytrace_mirror.reflect_factor
  else:
   mirror_factor = 0
  self.writel(S_FX, 6, '<color>' +
     numarr_alpha(material.mirror_color, mirror_factor) + '</color>')
  self.writel(S_FX, 5, '</reflective>')

  if (material.use_transparency):
   self.writel(S_FX, 5, '<transparency>')
   self.writel(S_FX, 6, '<float>' +
      strflt(material.alpha) + '</float>')
   self.writel(S_FX, 5, '</transparency>')

  if transparent_tex:
   self.writel(S_FX, 5, '<transparent>')
   self.writel(S_FX, 6, '<texture texture="' +
      sampler_sids[transparent_tex.pop()] + '" texcoord="UVMap"/>')
   self.writel(S_FX, 5, '</transparent>')

  self.writel(S_FX, 5, '<index_of_refraction>')
  self.writel(S_FX, 6, '<float>' +
     strflt(material.specular_ior) + '</float>')
  self.writel(S_FX, 5, '</index_of_refraction>')

  self.writel(S_FX, 4, '</' + shtype + '>')
  if (len(normal_tex)):
   self.writel(S_FX, 4, '<extra>')
   self.writel(S_FX, 5, '<technique profile="FCOLLADA">')
   self.writel(S_FX, 6, '<bump bumptype="NORMALMAP">')
   self.writel(S_FX, 7, '<texture texture="' +
      sampler_sids[normal_tex.pop()] + '" texcoord="UVMap"/>')
   self.writel(S_FX, 6, '</bump>')
   self.writel(S_FX, 5, '</technique>')
   self.writel(S_FX, 4, '</extra>')

  self.writel(S_FX, 3, '</technique>')
  self.writel(S_FX, 2, '</profile_COMMON>')
  self.writel(S_FX, 1, '</effect>')

 def get_mesh(self, node, force_modifiers=False):
  apply_modifiers = force_modifiers or self.use_mesh_modifiers

  # get a mesh for this node
  try:
   mesh = node.to_mesh(self.bpy_context_scene, apply_modifiers, "RENDER")
  except:
   return None

  if not mesh or not len(mesh.polygons):
   # mesh has no polygons so abort
   if mesh and (mesh != node.data):
    bpy.data.meshes.remove(mesh)
   return None

  # force triangulation if the mesh has polygons with more than 4 sides
  # corrupts custom normals
  force_triangluation = False
  if not self.triangulate:
   for polygon in mesh.polygons:
    if (polygon.loop_total > 4):
     force_triangluation = True
     break

  if (self.triangulate or force_triangluation):
   bm = bmesh.new()
   bm.from_mesh(mesh)
   bmesh.ops.triangulate(bm, faces=bm.faces)
   bm.to_mesh(mesh)
   bm.free()
   mesh.update(calc_tessface=True)

  calc_tangents = self.always_tangent
  if not calc_tangents and self.use_tangents:
   bump_texture = None
   try:
    instances = [
     n for n in self.visual_nodes if n.data == node.data]
    bump_texture = [
     tex_slot
     for i in instances
     for mat_slot in i.material_slots
     if mat_slot.material
     for tex_slot in mat_slot.material.texture_slots
     if tex_slot and tex_slot.use_map_normal]
   except:
    pass
   calc_tangents = bump_texture and len(bump_texture) > 0

  if not mesh.has_custom_normals and mesh.use_auto_smooth and not calc_tangents:
   mesh.calc_normals_split()

  if calc_tangents:
   try:
    mesh.calc_tangents()
   except:
    pass

  if (mesh != node.data):
   self.meshes_to_clear.append(mesh)
  return mesh

 def average_color(self, color, count):
  if count != 0:
   return Color(color / count).freeze()
  else:
   return Color(color).freeze()

 def loop_property_to_indexed(self, loop_vertices, prop):
  surface = {}
  pool = list({getattr(ml, prop).copy().freeze()
      for g in loop_vertices.values() for p in g for ml in p})
  if (not ((len(pool) == 1) and (pool[0].length < 0.1))):
   index_map = {k: v for (v, k) in enumerate(pool)}
   surface = {
    g: s for (g, s) in
    zip(loop_vertices.keys(),
     [[[index_map[getattr(v, prop).copy().freeze()] for v in p] for p in g]
      for g in loop_vertices.values()])}
  else:
   pool = []
  return pool, surface

 def get_mesh_surfaces(self, mesh):
  # Turn the mesh into buffers and index buffers, polygons are grouped by
  # material index

  # get vertices
  vertices = [Vector(v.co) for v in mesh.vertices.values()]

  # get polygons
  loop_vertices = self.get_polygon_groups(mesh)

  surface_v_indices = {
   g: s for (g, s) in
   zip(loop_vertices.keys(),
    [[[v.vertex_index for v in p] for p in g]
     for g in loop_vertices.values()])}

  normals = [v.normal.copy().freeze() for v in mesh.vertices.values()]

  split_normals = []
  surface_split_normals = {}
  if mesh.has_custom_normals or mesh.use_auto_smooth:
   split_normals, surface_split_normals = self.loop_property_to_indexed(
    loop_vertices, "normal")

  surface_tangent_indices = {}
  tangents = []
  surface_bitangent_indices = {}
  bitangents = []
  if self.use_tangents:
   tangents, surface_tangent_indices = self.loop_property_to_indexed(
    loop_vertices, "tangent")
   bitangents, surface_bitangent_indices = self.loop_property_to_indexed(
    loop_vertices, "bitangent")

  # get uv's
  if (mesh.uv_layers != None) and (mesh.uv_layers.active != None):
   uv_layer = mesh.uv_layers.active.data
  else:
   uv_layer = None

  uv = []
  if (uv_layer != None):
   # get all uv values, removing duplicates
   uv = list({uv.uv.copy().freeze() for uv in uv_layer.values()})

  surface_uv_indices = {}
  if (len(uv)):
   uv_map = {k: v for (v, k) in enumerate(uv)}
   # convert dictionary of loop vertices into a dictionary of uv
   # indices (that reference into the uv list)
   surface_uv_indices = {
    g: s for (g, s) in
    zip(loop_vertices.keys(),
     [[[uv_map[uv_layer[v.index].uv.copy().freeze()] for v in p] for p in g]
      for g in loop_vertices.values()])}

  surface_color_indices = {}
  if (mesh.vertex_colors):
   # Blender doesn't have colors per vertex instead color per polygon loop vertex.
   # So guess vertex colors by averaging every color used for each
   # vertex.
   color_buckets = [[0, Vector((0, 0, 0))]
        for i in range(len(vertices))]
   for loop_vertex in [v for g in loop_vertices.values() for p in g for v in p]:
    color_buckets[loop_vertex.vertex_index][0] += 1
    color_buckets[loop_vertex.vertex_index][1] += Vector(
     mesh.vertex_colors.active.data[loop_vertex.index].color)

   colors = [self.average_color(color, count)
       for (count, color) in color_buckets]
   opt_colors = list(set(colors))
   color_map = {k: v for (v, k) in enumerate(opt_colors)}
   surface_color_indices = {
    g: s for (g, s) in
    zip(loop_vertices.keys(),
     [[[color_map[colors[v.vertex_index]] for v in p] for p in g]
      for g in loop_vertices.values()])}
   colors = opt_colors
  else:
   colors = []

  # Pools of  values:
  # vertices = array of xyz point tuples
  # colors = array of r,g,b color tuples
  # normals = array of xyz vector tuples
  # split_normals = array of xyz vector tuples
  # uv = array of uv point tuples
  # tangents = array of xyz tangent tuples
  # bitangets = array of xyz bitangent tuples

  result = {}
  result['vertices'] = vertices
  result['colors'] = colors
  result['normals'] = normals
  result['split_normals'] = split_normals
  result['uv'] = uv
  result['tangents'] = tangents
  result['bitangents'] = bitangents

  # Polygon vertex index data
  # surface_v_indices = [material groups][polygons][index into vertices[] and normals[]]
  # surface_split_normals = [material groups][polygons][index into split_normals[]]
  # surface_color_indices = [material groups][polygons][index into colors[]]
  # surface_uv_indices = [material groups][polygons][index into uv[]]
  # surface_tangent_indices = [material groups][polygons][index into tangents[]]
  # surface_bitangents_indices = [material groups][polygons][index into
  # bitangents[]]

  result['surface_v_indices'] = surface_v_indices
  result['surface_split_normals'] = surface_split_normals
  result['surface_color_indices'] = surface_color_indices
  result['surface_uv_indices'] = surface_uv_indices
  result['surface_tangent_indices'] = surface_tangent_indices
  result['surface_bitangent_indices'] = surface_bitangent_indices

  return result

 def get_polygon_groups(self, mesh):
  # get a dictionary of polygons with loop vertices grouped by material
  vertices = {}

  for fi in range(len(mesh.polygons)):
   f = mesh.polygons[fi]

   # group by material index, retrieve the materials for each group

   if (not (f.material_index in vertices)):
    vertices[f.material_index] = []

   loop_vertices = vertices[f.material_index]

   polygon = []
   # output polygon indices
   for lt in range(f.loop_total):
    loop_index = f.loop_start + lt
    ml = mesh.loops[loop_index]
    polygon.append(ml)

   loop_vertices.append(polygon)

  # vertices[material index][polygons][MeshLoop]

  return vertices

 def node_has_convex_hull(self, node):
  if self.bpy_context_scene.render.engine == 'BLENDER_GAME':
   if (node.game.physics_type in self.valid_game_types) and node.game.use_collision_bounds and node.game.collision_bounds_type == 'CONVEX_HULL':
    return True
  else:
   if (node.rigid_body and
    node.rigid_body.collision_shape == 'CONVEX_HULL' and
     (node.rigid_body.mesh_source == 'BASE' or node.rigid_body.mesh_source == 'DEFORM')):
    return True
  return False

 def mesh_to_convex_hull(self, mesh):
  # use blender to generate a convex hull for the mesh

  # just need the vertices for hull generation

  bm = bmesh.new()
  for vert in mesh.vertices.values():
   bm.verts.new(vert.co)
  bm.verts.ensure_lookup_table()

  try:
   # Generate hull
   ret = bmesh.ops.convex_hull(
    bm, input=bm.verts, use_existing_faces=False)

   # Delete the vertices that weren't used in the convex hull

   geom = ret["geom_unused"]
   # there is no documentation for values of 'context'. 1 works to
   # delete vertices
   bmesh.ops.delete(bm, geom=geom, context=1)
  except:
   return None

  # convert bmesh back to mesh
  me = bpy.data.meshes.new(self.new_id("DAE_convex_hull"))
  bm.to_mesh(me)
  if (me != mesh):
   self.meshes_to_clear.append(me)
  return me

 def export_meshes(self, lookup):
  geometry_lookup = lookup["mesh"]
  convex_geometry_lookup = lookup["convex_mesh"]
  node_to_mesh_lookup=lookup["node_to_mesh"]

  mesh_nodes = {node for node in self.bpy_context_scene.objects if (node.type == "MESH" or node.type ==
      "CURVE") and node not in node_to_mesh_lookup and node in self.visual_nodes}

  for node in mesh_nodes:

   has_modifiers=self.node_has_generate_modifiers(node)
   if (has_modifiers  or node.data not in geometry_lookup):
    # generate mesh from node

    mesh = self.get_mesh(node)

    # export the mesh
    if (mesh):
     mesh_id = self.get_node_id(node.data.name + "-mesh")
     material_bind = self.export_mesh(
      mesh, mesh_id, node.data.name)

     # export convex hull if needed by physics scene

     if (self.node_has_convex_hull(node)):
      convex_mesh = self.mesh_to_convex_hull(mesh)
      if (convex_mesh):
       convex_mesh_id = mesh_id + "-convex"
       self.export_mesh(
        convex_mesh, convex_mesh_id, node.data.name, True)
       convex_geometry_lookup[node.data] = convex_mesh_id

     # export morphs from shape keys

     morphs = self.export_mesh_morphs(node, mesh_id)

     lookup={"id":mesh_id,"material_bind":material_bind,"morphs": morphs}
     node_to_mesh_lookup[node]=lookup

     # Modifiers will generate a different mesh even if it uses the same mesh data
     if not has_modifiers:
      geometry_lookup[node.data]=lookup

    else:
     if(node.type == "CURVE"):
      # All else failed so export a Bezier curve
      curve_id = self.get_node_id(node.data.name + "-curve")
      self.export_curve(node.data, curve_id)
      lookup={"id":curve_id,"material_bind":None,"morphs": None}
      geometry_lookup[node.data] = lookup
   else:
    node_to_mesh_lookup[node]=geometry_lookup[node.data]

  self.remove_export_meshes()



 def export_mesh_morphs(self, node, mesh_id):
  mesh = node.data

  if self.mesh_has_morphs(mesh):
   values = []
   # save weight values for restoration after being monkeyed with

   for k in range(1, len(mesh.shape_keys.key_blocks)):
    shape = node.data.shape_keys.key_blocks[k]
    values += [shape.value]
    shape.value = 0

   scene_show_only_shape_key = node.show_only_shape_key
   scene_active_shape_key = node.active_shape_key_index

   morph_targets = []

   # pose the mesh using each shape key and export a mesh for each

   for k in range(1, len(mesh.shape_keys.key_blocks)):
    shape = node.data.shape_keys.key_blocks[k]
    node.show_only_shape_key = True
    node.active_shape_key_index = k
    shape.value = 1.0
    mesh.update()
    morph_id = self.get_node_id(node.data.name + "-" + shape.name)
    morph_targets.append([morph_id, values[k - 1]])
    export_mesh = self.get_mesh(node,force_modifiers=True)
    self.export_mesh(export_mesh, morph_id, morph_id, morph=True)
    shape.value = values[k - 1]

   node.show_only_shape_key = scene_show_only_shape_key
   node.active_shape_key_index = scene_active_shape_key
   mesh.update()

   # morph_targets = [id of morph shape mesh, the shape key index
   # corrected to remove the basis (shape key 1 is index 0)]
   return morph_targets
  else:
   return None

 def export_morph_controllers(self, lookup):
  node_to_mesh_lookup=lookup["node_to_mesh"]
  morph_controller_lookup = lookup["mesh_to_morph_controller"]
  geometry_morphs={node:value for node,value in node_to_mesh_lookup.items() if value["morphs"]!=None and node not in morph_controller_lookup and node in self.visual_nodes}
  for node,values in geometry_morphs.items():
   if node.data not in morph_controller_lookup:
    morph_id = self.get_node_id(node.data.name + "-morph")
    targets=values["morphs"]
    morph_controller_lookup[node.data] = {"id":morph_id, "targets":targets}
    self.export_morph_controller(values["id"], targets, morph_id)

 def export_morph_controller(self, mesh_id, morph_targets, morph_id):
  self.writel(S_MORPH, 1, '<controller id="' +
     morph_id + '" name="' + morph_id + '">')
  self.writel(S_MORPH, 2, '<morph source="' +
     self.ref_id(mesh_id) + '" method="NORMALIZED">')
  self.writel(S_MORPH, 3, '<source id="' + morph_id + '-targets">')
  self.writel(S_MORPH, 4, '<IDREF_array id="' + morph_id +
     '-targets-array" count="' + str(len(morph_targets)) + '">')
  marr = " ".join([self.quote_spaces(name[0]) for name in morph_targets])
  warr = " ".join([strflt(weight[1]) for weight in morph_targets])
  self.writel(S_MORPH, 5, marr)
  self.writel(S_MORPH, 4, '</IDREF_array>')
  self.writel(S_MORPH, 4, '<technique_common>')
  self.writel(S_MORPH, 5, '<accessor source="' + self.ref_id(morph_id) +
     '-targets-array" count="' + str(len(morph_targets)) + '" stride="1">')
  self.writel(S_MORPH, 6, '<param name="MORPH_TARGET" type="IDREF"/>')
  self.writel(S_MORPH, 5, '</accessor>')
  self.writel(S_MORPH, 4, '</technique_common>')
  self.writel(S_MORPH, 3, '</source>')
  self.writel(S_MORPH, 3, '<source id="' + morph_id + '-weights">')
  self.writel(S_MORPH, 4, '<float_array id="' + morph_id +
     '-weights-array" count="' + str(len(morph_targets)) + '" >')
  self.writel(S_MORPH, 5, warr)
  self.writel(S_MORPH, 4, '</float_array>')
  self.writel(S_MORPH, 4, '<technique_common>')
  self.writel(S_MORPH, 5, '<accessor source="' + self.ref_id(morph_id) +
     '-weights-array" count="' + str(len(morph_targets)) + '" stride="1">')
  self.writel(S_MORPH, 6, '<param name="MORPH_WEIGHT" type="float"/>')
  self.writel(S_MORPH, 5, '</accessor>')
  self.writel(S_MORPH, 4, '</technique_common>')
  self.writel(S_MORPH, 3, '</source>')
  self.writel(S_MORPH, 3, '<targets>')
  self.writel(
   S_MORPH, 4, '<input semantic="MORPH_TARGET" source="' + self.ref_id(morph_id) + '-targets"/>')
  self.writel(
   S_MORPH, 4, '<input semantic="MORPH_WEIGHT" source="' + self.ref_id(morph_id) + '-weights"/>')
  self.writel(S_MORPH, 3, '</targets>')
  self.writel(S_MORPH, 2, '</morph>')
  self.writel(S_MORPH, 1, '</controller>')

 def node_has_generate_modifiers(self,node):
  if hasattr(node,"modifiers"):
   has=next((mod
             for mod in node.modifiers
             if mod.show_render and
             mod.type != "ARMATURE"
             ),None)!= None
  else:
   has = False

  return has

 def node_skin_modifiers(self,node):
  if hasattr(node,"modifiers"):
   return (mod for mod in node.modifiers if mod.show_render and mod.type=="ARMATURE" and mod.use_vertex_groups)
  else:
   return ()

 def node_has_skin_modifier(self,node):
  return next((self.node_skin_modifiers(node)),None)!=None

 def export_skin_controllers(self, lookup):

  morph_controller_lookup = lookup["mesh_to_morph_controller"]
  skin_controller_lookup = lookup["node_to_skin"]
  node_to_mesh_lookup =lookup["node_to_mesh"]

  meshes = {node
            for node in self.bpy_context_scene.objects
            if node in self.visual_nodes and
            node not in skin_controller_lookup and
            self.node_has_skin_modifier(node)}

  for node in meshes:
   armatures = self.node_skin_modifiers(node)
   for armature in armatures:
    skin_id = self.get_node_id(
     node.name + "-" + armature.object.name + "-skin")
    lu = {"skin": skin_id, "skeleton": armature.object.name}
    if (not node in skin_controller_lookup):
     skin_controller_lookup[node] = []
    skin_controller_lookup[node].append(lu)
    mesh_id=node_to_mesh_lookup[node]["id"]
    morph=morph_controller_lookup.get(node.data,None)
    if morph:
     attached_id=morph["id"]
    else:
     attached_id=mesh_id;
    self.export_skin_controller(
     node, armature.object, attached_id, skin_id)

 def export_skin_controller(self, node, armature, mesh_id, skin_id):

  if not self.overstuff_bones:
   group_names = [group.name for group in node.vertex_groups.values(
   ) if group.name in armature.data.bones]
  else:
   # put every bone from the armature into the skin because reasons

   group_names = [group for group in armature.data.bones.keys()]

  missing_group_names = {group.name for group in node.vertex_groups.values(
  ) if group.name not in armature.data.bones}
  group_names_index = dict({k: v for (v, k) in enumerate(group_names)}.items() | {
   k:-1 for k in missing_group_names}.items())

  bones = [armature.data.bones[name] for name in group_names]
  pose_matrices = [
   (armature.matrix_world * bone.matrix_local).inverted() for bone in bones]
  mesh=self.get_mesh(node)
  weights = list(
   {group.weight for v in mesh.vertices for group in v.groups})
  weights_index = {k: v for (v, k) in enumerate(weights)}
  vertex_weights = [[[group_names_index[node.vertex_groups[g.group].name],
       weights_index[g.weight]] for g in v.groups] for v in mesh.vertices]
  vertex_weights = [[g for g in v if g[0] != -1] for v in vertex_weights]
  weight_counts = [len(v) for v in vertex_weights]

  self.writel(S_SKIN, 1, '<controller id="' + skin_id + '">')
  self.writel(S_SKIN, 2, '<skin source="' + self.ref_id(mesh_id) + '">')
  self.writel(S_SKIN, 3, '<bind_shape_matrix>' +
     self.strmtx(node.matrix_world) + '</bind_shape_matrix>')

  # Joint Names

  self.writel(S_SKIN, 3, '<source id="' + skin_id + '-joints">')
  name_values = " ".join([self.quote_spaces(name)
        for name in group_names])
  self.writel(S_SKIN, 4, '<Name_array id="' + skin_id + '-joints-array" count="' +
     str(len(group_names)) + '">' + name_values + '</Name_array>')
  self.writel(S_SKIN, 4, '<technique_common>')
  self.writel(S_SKIN, 4, '<accessor source="' + self.ref_id(skin_id) +
     '-joints-array" count="' + str(len(group_names)) + '" stride="1">')
  self.writel(S_SKIN, 5, '<param name="JOINT" type="Name"/>')
  self.writel(S_SKIN, 4, '</accessor>')
  self.writel(S_SKIN, 4, '</technique_common>')
  self.writel(S_SKIN, 3, '</source>')

  # Pose Matrices!

  self.writel(S_SKIN, 3, '<source id="' + skin_id + '-bind_poses">')
  pose_values = " ".join([self.strmtx(matrix)
        for matrix in pose_matrices])
  self.writel(S_SKIN, 4, '<float_array id="' + skin_id + '-bind_poses-array" count="' +
     str(len(pose_matrices) * 16) + '">' + pose_values + '</float_array>')
  self.writel(S_SKIN, 4, '<technique_common>')
  self.writel(S_SKIN, 4, '<accessor source="' + self.ref_id(skin_id) +
     '-bind_poses-array" count="' + str(len(pose_matrices)) + '" stride="16">')
  self.writel(S_SKIN, 5, '<param name="TRANSFORM" type="float4x4"/>')
  self.writel(S_SKIN, 4, '</accessor>')
  self.writel(S_SKIN, 4, '</technique_common>')
  self.writel(S_SKIN, 3, '</source>')

  # Skin Weights!

  self.writel(S_SKIN, 3, '<source id="' + skin_id + '-weights">')
  self.writel(S_SKIN, 4, '<float_array id="' + skin_id + '-weights-array" count="' +
     str(len(weights)) + '">' + " ".join([strflt(w) for w in weights]) + '</float_array>')
  self.writel(S_SKIN, 4, '<technique_common>')
  self.writel(S_SKIN, 4, '<accessor source="' + self.ref_id(skin_id) +
     '-weights-array" count="' + str(len(weights)) + '" stride="1">')
  self.writel(S_SKIN, 5, '<param name="WEIGHT" type="float"/>')
  self.writel(S_SKIN, 4, '</accessor>')
  self.writel(S_SKIN, 4, '</technique_common>')
  self.writel(S_SKIN, 3, '</source>')
  self.writel(S_SKIN, 3, '<joints>')
  self.writel(S_SKIN, 4, '<input semantic="JOINT" source="' +
     self.ref_id(skin_id) + '-joints"/>')
  self.writel(
   S_SKIN, 4, '<input semantic="INV_BIND_MATRIX" source="' + self.ref_id(skin_id) + '-bind_poses"/>')
  self.writel(S_SKIN, 3, '</joints>')
  self.writel(S_SKIN, 3, '<vertex_weights count="' +
     str(len(weight_counts)) + '">')
  self.writel(S_SKIN, 4, '<input semantic="JOINT" source="' +
     self.ref_id(skin_id) + '-joints" offset="0"/>')
  self.writel(S_SKIN, 4, '<input semantic="WEIGHT" source="' +
     self.ref_id(skin_id) + '-weights" offset="1"/>')
  self.writel(S_SKIN, 4, '<vcount>' +
     " ".join([str(c) for c in weight_counts]) + '</vcount>')
  self.writel(S_SKIN, 4, '<v>' +
     " ".join([str(i) for v in vertex_weights for g in v for i in g]) + '</v>')
  self.writel(S_SKIN, 3, '</vertex_weights>')
  self.writel(S_SKIN, 2, '</skin>')
  self.writel(S_SKIN, 1, '</controller>')

 def mesh_has_morphs(self, mesh):
  return (mesh.shape_keys and len(mesh.shape_keys.key_blocks))

 def export_mesh(self, mesh, mesh_id, mesh_name, convex=False, morph=False):

  surfaces = self.get_mesh_surfaces(mesh)

  vertices = surfaces['vertices']
  colors = surfaces['colors']
  normals = surfaces['normals']
  split_normals = surfaces['split_normals']
  uv = surfaces['uv']
  tangents = surfaces['tangents']
  bitangents = surfaces['bitangents']
  surface_v_indices = surfaces['surface_v_indices']
  surface_split_normals = surfaces['surface_split_normals']
  surface_color_indices = surfaces['surface_color_indices']
  surface_uv_indices = surfaces['surface_uv_indices']
  surface_tangent_indices = surfaces['surface_tangent_indices']
  surface_bitangent_indices = surfaces['surface_bitangent_indices']

  has_vertex = len(vertices) > 0
  has_split_normals = not morph and (len(split_normals) > 0) and not self.mesh_has_morphs(mesh)
  if has_split_normals:
   normals = split_normals
   has_normals = True
  else:
   has_normals = len(normals) > 0
  has_uv = not morph and (len(uv) > 0)
  has_colors = not morph and (has_vertex and len(colors) > 0)
  has_tangents = not morph and (len(tangents) > 0)
  has_bitangents = not morph and (has_normals and has_tangents and (len(bitangents) > 0))

  self.writel(S_GEOM, 1, '<geometry id="' +
     mesh_id + '" name="' + mesh_name + '">')
  if (convex):
   self.writel(S_GEOM, 2, '<convex_mesh>')
  else:
   self.writel(S_GEOM, 2, '<mesh>')

  # Vertex Array
  if has_vertex:
   self.writel(S_GEOM, 3, '<source id="' + mesh_id + '-positions">')
   float_values = " ".join([self.strxyz(v) for v in vertices])
   self.writel(S_GEOM, 4, '<float_array id="' + mesh_id + '-positions-array" count="' +
      str(len(vertices) * 3) + '">' + float_values + '</float_array>')
   self.writel(S_GEOM, 4, '<technique_common>')
   self.writel(S_GEOM, 5, '<accessor source="' + self.ref_id(mesh_id) +
      '-positions-array" count="' + str(len(vertices)) + '" stride="3">')
   self.writel(S_GEOM, 6, '<param name="X" type="float"/>')
   self.writel(S_GEOM, 6, '<param name="Y" type="float"/>')
   self.writel(S_GEOM, 6, '<param name="Z" type="float"/>')
   self.writel(S_GEOM, 5, '</accessor>')
   self.writel(S_GEOM, 4, '</technique_common>')
   self.writel(S_GEOM, 3, '</source>')

  # Normal Array
  if has_normals:
   self.writel(S_GEOM, 3, '<source id="' + mesh_id + '-normals">')
   float_values = " ".join([self.strxyz(v) for v in normals])
   self.writel(S_GEOM, 4, '<float_array id="' + mesh_id + '-normals-array" count="' +
      str(len(normals) * 3) + '">' + float_values + '</float_array>')
   self.writel(S_GEOM, 4, '<technique_common>')
   self.writel(S_GEOM, 5, '<accessor source="' + self.ref_id(mesh_id) +
      '-normals-array" count="' + str(len(normals)) + '" stride="3">')
   self.writel(S_GEOM, 6, '<param name="X" type="float"/>')
   self.writel(S_GEOM, 6, '<param name="Y" type="float"/>')
   self.writel(S_GEOM, 6, '<param name="Z" type="float"/>')
   self.writel(S_GEOM, 5, '</accessor>')
   self.writel(S_GEOM, 4, '</technique_common>')
   self.writel(S_GEOM, 3, '</source>')

  # UV Arrays
  if has_uv:
   self.writel(S_GEOM, 3, '<source id="' + mesh_id + '-texcoord">')
   float_values = " ".join(
    [strflt(c) for v in [[v.x, v.y] for v in uv] for c in v])
   self.writel(S_GEOM, 4, '<float_array id="' + mesh_id + '-texcoord-array" count="' +
      str(len(uv) * 2) + '">' + float_values + '</float_array>')
   self.writel(S_GEOM, 4, '<technique_common>')
   self.writel(S_GEOM, 5, '<accessor source="' + self.ref_id(mesh_id) +
      '-texcoord-array" count="' + str(len(uv)) + '" stride="2">')
   self.writel(S_GEOM, 6, '<param name="S" type="float"/>')
   self.writel(S_GEOM, 6, '<param name="T" type="float"/>')
   self.writel(S_GEOM, 5, '</accessor>')
   self.writel(S_GEOM, 4, '</technique_common>')
   self.writel(S_GEOM, 3, '</source>')

  # Color Arrays

  if has_colors:
   self.writel(S_GEOM, 3, '<source id="' + mesh_id + '-colors">')
   float_values = " ".join(
    [strflt(c) for v in [[v.r, v.g, v.b] for v in colors] for c in v])
   self.writel(S_GEOM, 4, '<float_array id="' + mesh_id + '-colors-array" count="' +
      str(len(colors) * 3) + '">' + float_values + '</float_array>')
   self.writel(S_GEOM, 4, '<technique_common>')
   self.writel(S_GEOM, 5, '<accessor source="' + self.ref_id(mesh_id) +
      '-colors-array" count="' + str(len(colors)) + '" stride="3">')
   self.writel(S_GEOM, 6, '<param name="R" type="float"/>')
   self.writel(S_GEOM, 6, '<param name="G" type="float"/>')
   self.writel(S_GEOM, 6, '<param name="B" type="float"/>')
   self.writel(S_GEOM, 5, '</accessor>')
   self.writel(S_GEOM, 4, '</technique_common>')
   self.writel(S_GEOM, 3, '</source>')

  if has_tangents:
   self.writel(
    S_GEOM, 3, "<source id=\"{}-tangents\">".format(mesh_id))
   float_values = " ".join([self.strxyz(v) for v in tangents])
   self.writel(
    S_GEOM, 4, "<float_array id=\"{}-tangents-array\" "
    "count=\"{}\">{}</float_array>".format(
     mesh_id, len(tangents) * 3, float_values))
   self.writel(S_GEOM, 4, "<technique_common>")
   self.writel(
    S_GEOM, 4, "<accessor source=\"{}-tangents-array\" "
    "count=\"{}\" stride=\"3\">".format(self.ref_id(mesh_id), len(tangents)))
   self.writel(S_GEOM, 5, "<param name=\"X\" type=\"float\"/>")
   self.writel(S_GEOM, 5, "<param name=\"Y\" type=\"float\"/>")
   self.writel(S_GEOM, 5, "<param name=\"Z\" type=\"float\"/>")
   self.writel(S_GEOM, 4, "</accessor>")
   self.writel(S_GEOM, 4, "</technique_common>")
   self.writel(S_GEOM, 3, "</source>")

  if has_bitangents:
   self.writel(S_GEOM, 3, "<source id=\"{}-bitangents\">".format(
    mesh_id))
   float_values = " ".join([self.strxyz(v) for v in bitangents])
   self.writel(
    S_GEOM, 4, "<float_array id=\"{}-bitangents-array\" "
    "count=\"{}\">{}</float_array>".format(
     mesh_id, len(bitangents) * 3, float_values))
   self.writel(S_GEOM, 4, "<technique_common>")
   self.writel(
    S_GEOM, 4, "<accessor source=\"{}-bitangents-array\" "
    "count=\"{}\" stride=\"3\">".format(self.ref_id(mesh_id), len(bitangents)))
   self.writel(S_GEOM, 5, "<param name=\"X\" type=\"float\"/>")
   self.writel(S_GEOM, 5, "<param name=\"Y\" type=\"float\"/>")
   self.writel(S_GEOM, 5, "<param name=\"Z\" type=\"float\"/>")
   self.writel(S_GEOM, 4, "</accessor>")
   self.writel(S_GEOM, 4, "</technique_common>")
   self.writel(S_GEOM, 3, "</source>")

  # Vertices
  self.writel(S_GEOM, 3, '<vertices id="' + mesh_id + '-vertices">')
  if (has_vertex):
   self.writel(
    S_GEOM, 4, '<input semantic="POSITION" source="' + self.ref_id(mesh_id) + '-positions"/>')
  if (has_normals and not has_split_normals):
   self.writel(
    S_GEOM, 4, '<input semantic="NORMAL" source="' + self.ref_id(mesh_id) + '-normals"/>')
  self.writel(S_GEOM, 3, '</vertices>')

  material_bind = {}

  if not morph:

   # calculate offsets and layout of <p> indices
   offset = 0
   if (has_vertex):
    vertex_offset = offset
    offset += 1
   if (has_split_normals):
    normal_offset = offset
    offset += 1
   elif has_normals:
    normal_offset = vertex_offset
   if has_tangents:
    tangent_offset = offset
    offset += 1
   if has_bitangents:
    bitangent_offset = offset
    offset += 1
   if (has_colors):
    color_offset = offset
    offset += 1
   if (has_uv):
    uv_offset = offset
    offset += 1

   stride = offset

   for mat_index, polygons in surface_v_indices.items():

    # Triangle Lists
    triangulated = self.triangulate or not next(
     (p for p in polygons if len(p) != 3), False)
    if (triangulated):
     prim_type = "triangles"
    else:
     prim_type = "polylist"

    # Every renderable mesh must have a material symbol even if no
    # material is assigned in Blender.
    matref = "symbol-" + str(mat_index)
    material_bind[mat_index] = matref

    self.writel(S_GEOM, 3, '<' + prim_type + ' count="' +
       str(len(polygons)) + '" material="' + matref + '">')  # todo material
    if (has_vertex):
     self.writel(S_GEOM, 4, '<input semantic="VERTEX" source="' +
        self.ref_id(mesh_id) + '-vertices" offset="' + str(vertex_offset) + '"/>')
    if (has_normals):
     self.writel(S_GEOM, 4, '<input semantic="NORMAL" source="' +
        self.ref_id(mesh_id) + '-normals" offset="' + str(normal_offset) + '"/>')
    if (has_tangents):
     self.writel(S_GEOM, 4, '<input semantic="TEXTANGENT" source="' +
        self.ref_id(mesh_id) + '-tangents" offset="' + str(tangent_offset) + '" set="0"/>')
    if (has_bitangents):
     self.writel(S_GEOM, 4, '<input semantic="TEXBINORMAL" source="' +
        self.ref_id(mesh_id) + '-bitangents" offset="' + str(bitangent_offset) + '" set="0"/>')
    if (has_colors):
     self.writel(S_GEOM, 4, '<input semantic="COLOR" source="' +
        self.ref_id(mesh_id) + '-colors" offset="' + str(color_offset) + '"/>')
    if (has_uv):
     self.writel(S_GEOM, 4, '<input semantic="TEXCOORD" source="' +
        self.ref_id(mesh_id) + '-texcoord" offset="' + str(uv_offset) + '" set="0"/>')

    # vcount list if not triangulating, as a triangle always has 3
    # sides no need for an array of 3s
    if prim_type == "polylist":
     int_values = "<vcount>"
     int_values += " ".join([str(len(p)) for p in polygons])
     int_values += "</vcount>"
     self.writel(S_GEOM, 4, int_values)

    # faces
    int_values = "<p>"

    index_buffer = []
    indices = [0 for i in range(stride)]
    for p in range(0, len(polygons)):
     uv_indices = None
     for i in range(0, len(polygons[p])):
      if has_vertex:
       indices[vertex_offset] = polygons[p][i]
      if has_split_normals:
       indices[normal_offset] = surface_split_normals[mat_index][p][i]
      if has_colors:
       indices[color_offset] = surface_color_indices[mat_index][p][i]
      if has_uv:
       indices[uv_offset] = surface_uv_indices[mat_index][p][i]
      if has_tangents:
       indices[tangent_offset] = surface_tangent_indices[mat_index][p][i]
      if has_bitangents:
       indices[bitangent_offset] = surface_bitangent_indices[mat_index][p][i]
      index_buffer.append(indices.copy())
    int_values += " ".join([str(i)
          for c in index_buffer for i in c])
    int_values += "</p>"
    self.writel(S_GEOM, 4, int_values)
    self.writel(S_GEOM, 3, '</' + prim_type + '>')
  if (convex):
   self.writel(S_GEOM, 2, '</convex_mesh>')
  else:
   self.writel(S_GEOM, 2, '</mesh>')
  self.writel(S_GEOM, 1, '</geometry>')

  # Key: material slot index, Value: the link target symbol name
  return material_bind

 def get_node_id(self, name):
  count = self.node_names.get(name, 0)
  node_id = name
  if (count > 0):
   node_id += "-" + str(count)
  self.node_names[name] = count + 1
  return node_id

 def export_armature_bone(self, section, bone, il, si, lookup, parenting_map, recurse=True):

  boneid = self.get_node_id(bone.name)
  bonesid = bone.name
  si[bone] = boneid
  self.writel(section, il, '<node id="' + boneid + '" sid="' +
     bonesid + '" name="' + bone.name + '" type="JOINT">')
  il += 1

  # don't put a transform if the bone doesn't deform

  transforms = self.get_bone_transform_xml(bone)
  for t in transforms:
   self.writel(section, il, t)

  # export nodes that are parented to this bone

  node_children = parenting_map.get(bone.name, None)
  if (recurse and node_children):
   for c in node_children:
    self.export_node(section, c, il, lookup, recurse)

  for c in bone.children:
   self.export_armature_bone(
    section, c, il, si, lookup, parenting_map, recurse)
  il -= 1
  self.writel(section, il, '</node>')

 def save_scene_pose(self):
  self.pose_state = {}
  for node in self.bpy_context_scene.objects:
   if node.data and hasattr(node.data, 'pose_position'):
    pose_position = node.data.pose_position
    self.pose_state[node] = pose_position

 def rest_scene(self):
  for node in self.bpy_context_scene.objects:
   if node.data and hasattr(node.data, 'pose_position'):
    pose_position = node.data.pose_position
    if (pose_position == 'POSE'):
     node.data.pose_position = 'REST'
  self.bpy_context_scene.update()
  self.settle_ik()

 def restore_scene_pose(self):
  for node, pose_position in self.pose_state.items():
   node.data.pose_position = pose_position
  self.bpy_context_scene.update()

 def export_armature_node(self, section, node, il, lookup, recurse=True):
  parenting_map = self.get_armature_children_of_bones(node)

  armature = node.data

  lookup["skeleton_info"][node] = {}

  for b in armature.bones:
   if (b.parent != None):
    # this node will be exported when the parent exports its
    # children
    continue
   self.export_armature_bone(
    section, b, il, lookup["skeleton_info"][node], lookup, parenting_map, recurse)

 def export_cameras(self, lookup):
  camera_lookup = lookup["camera"]
  cameras = {node.data for node in self.bpy_context_scene.objects if node.type ==
       "CAMERA" and node in self.visual_nodes and node.data not in camera_lookup}

  for camera in cameras:
   camera_id = self.get_node_id(camera.name + "-camera")
   camera_lookup[camera] = camera_id
   self.export_camera(camera, camera_id)

 def export_camera(self, camera, camera_id):

  self.writel(S_CAMS, 1, '<camera id="' + camera_id +
     '" name="' + camera.name + '">')
  self.writel(S_CAMS, 2, '<optics>')
  self.writel(S_CAMS, 3, '<technique_common>')
  if (camera.type == "PERSP"):
   self.writel(S_CAMS, 4, '<perspective>')
   self.writel(S_CAMS, 5, '<yfov> ' +
      strflt(math.degrees(camera.angle)) + ' </yfov>')  # I think?
   self.writel(S_CAMS, 5, '<aspect_ratio> ' + strflt(self.bpy_context_scene.render.resolution_x /
                 self.bpy_context_scene.render.resolution_y) + ' </aspect_ratio>')
   self.writel(S_CAMS, 5, '<znear> ' +
      strflt(camera.clip_start) + ' </znear>')
   self.writel(S_CAMS, 5, '<zfar> ' +
      strflt(camera.clip_end) + ' </zfar>')
   self.writel(S_CAMS, 4, '</perspective>')
  else:
   self.writel(S_CAMS, 4, '<orthographic>')
   self.writel(S_CAMS, 5, '<xmag> ' +
      strflt(camera.ortho_scale * 0.5) + ' </xmag>')  # I think?
   self.writel(S_CAMS, 5, '<aspect_ratio> ' + strflt(self.bpy_context_scene.render.resolution_x /
                 self.bpy_context_scene.render.resolution_y) + ' </aspect_ratio>')
   self.writel(S_CAMS, 5, '<znear> ' +
      strflt(camera.clip_start) + ' </znear>')
   self.writel(S_CAMS, 5, '<zfar> ' +
      strflt(camera.clip_end) + ' </zfar>')
   self.writel(S_CAMS, 4, '</orthographic>')

  self.writel(S_CAMS, 3, '</technique_common>')
  self.writel(S_CAMS, 2, '</optics>')
  self.writel(S_CAMS, 1, '</camera>')

 def export_lights(self, lookup):
  light_lookup = lookup["light"]
  lights = {node.data for node in self.bpy_context_scene.objects if node.type ==
      "LAMP" and node in self.visual_nodes and node.data not in light_lookup}
  for light in lights:
   light_id = self.get_node_id(light.name + "-light")
   light_lookup[light] = light_id
   self.export_lamp(light, light_id)

  return light_lookup

 def export_lamp(self, light, light_id):

  self.writel(S_LAMPS, 1, '<light id="' + light_id +
     '" name="' + light.name + '">')
  # self.writel(S_LAMPS,2,'<optics>')
  self.writel(S_LAMPS, 3, '<technique_common>')

  if (light.type == "POINT"):
   self.writel(S_LAMPS, 4, '<point>')
   self.writel(S_LAMPS, 5, '<color>' +
      strarr(light.color) + '</color>')
   att_by_distance = 2.0 / light.distance  # convert to linear attenuation
   self.writel(S_LAMPS, 5, '<linear_attenuation>' +
      strflt(att_by_distance) + '</linear_attenuation>')
   if (light.use_sphere):
    self.writel(S_LAMPS, 5, '<zfar>' +
       strflt(light.distance) + '</zfar>')
   self.writel(S_LAMPS, 4, '</point>')
  elif (light.type == "SPOT"):
   self.writel(S_LAMPS, 4, '<spot>')
   self.writel(S_LAMPS, 5, '<color>' +
      strarr(light.color) + '</color>')
   att_by_distance = 2.0 / light.distance  # convert to linear attenuation
   self.writel(S_LAMPS, 5, '<linear_attenuation>' +
      strflt(att_by_distance) + '</linear_attenuation>')
   self.writel(S_LAMPS, 5, '<falloff_angle>' +
      strflt(math.degrees(light.spot_size / 2)) + '</falloff_angle>')
   self.writel(S_LAMPS, 4, '</spot>')
  else:  # write a sun lamp for everything else (not supported)
   self.writel(S_LAMPS, 4, '<directional>')
   self.writel(S_LAMPS, 5, '<color>' +
      strarr(light.color) + '</color>')
   self.writel(S_LAMPS, 4, '</directional>')

  self.writel(S_LAMPS, 3, '</technique_common>')
  # self.writel(S_LAMPS,2,'</optics>')
  self.writel(S_LAMPS, 1, '</light>')

 def export_curve(self, curve, spline_id):

  self.writel(S_GEOM, 1, '<geometry id="' +
     spline_id + '" name="' + curve.name + '">')
  self.writel(S_GEOM, 2, '<spline closed="0">')

  points = []
  interps = []
  handles_in = []
  handles_out = []
  tilts = []

  for cs in curve.splines:
   if (cs.type == "BEZIER"):
    for s in cs.bezier_points:
     points.append(s.co.copy().freeze())
     handles_in.append(s.handle_left.copy().freeze())
     handles_out.append(s.handle_right.copy().freeze())
     tilts.append(s.tilt)
     interps.append("BEZIER")
   else:

    for s in cs.points:
     points.append(s.co.copy().freeze())
     handles_in.append(s.co.copy().freeze())
     handles_out.append(s.co.copy().freeze())
     tilts.append(s.tilt)
     interps.append("LINEAR")

  self.writel(S_GEOM, 3, '<source id="' + spline_id + '-positions">')
  position_values = " ".join([self.strxyz(x) for x in points])
  self.writel(S_GEOM, 4, '<float_array id="' + spline_id + '-positions-array" count="' +
     str(len(points) * 3) + '">' + position_values + '</float_array>')
  self.writel(S_GEOM, 4, '<technique_common>')
  self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(spline_id) +
     '-positions-array" count="' + str(len(points)) + '" stride="3">')
  self.writel(S_GEOM, 5, '<param name="X" type="float"/>')
  self.writel(S_GEOM, 5, '<param name="Y" type="float"/>')
  self.writel(S_GEOM, 5, '<param name="Z" type="float"/>')
  self.writel(S_GEOM, 4, '</accessor>')
  self.writel(S_GEOM, 4, '</technique_common>')
  self.writel(S_GEOM, 3, '</source>')
  self.writel(S_GEOM, 3, '<source id="' + spline_id + '-intangents">')
  intangent_values = " ".join([self.strxyz(x) for x in handles_in])
  self.writel(S_GEOM, 4, '<float_array id="' + spline_id + '-intangents-array" count="' +
     str(len(points) * 3) + '">' + intangent_values + '</float_array>')
  self.writel(S_GEOM, 4, '<technique_common>')
  self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(spline_id) +
     '-intangents-array" count="' + str(len(points)) + '" stride="3">')
  self.writel(S_GEOM, 5, '<param name="X" type="float"/>')
  self.writel(S_GEOM, 5, '<param name="Y" type="float"/>')
  self.writel(S_GEOM, 5, '<param name="Z" type="float"/>')
  self.writel(S_GEOM, 4, '</accessor>')
  self.writel(S_GEOM, 4, '</technique_common>')
  self.writel(S_GEOM, 3, '</source>')
  self.writel(S_GEOM, 3, '<source id="' + spline_id + '-outtangents">')
  outtangent_values = " ".join([self.strxyz(x) for x in handles_out])
  self.writel(S_GEOM, 4, '<float_array id="' + spline_id + '-outtangents-array" count="' +
     str(len(points) * 3) + '">' + outtangent_values + '</float_array>')
  self.writel(S_GEOM, 4, '<technique_common>')
  self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(spline_id) +
     '-outtangents-array" count="' + str(len(points)) + '" stride="3">')
  self.writel(S_GEOM, 5, '<param name="X" type="float"/>')
  self.writel(S_GEOM, 5, '<param name="Y" type="float"/>')
  self.writel(S_GEOM, 5, '<param name="Z" type="float"/>')
  self.writel(S_GEOM, 4, '</accessor>')
  self.writel(S_GEOM, 4, '</technique_common>')
  self.writel(S_GEOM, 3, '</source>')
  self.writel(S_GEOM, 3, '<source id="' +
     spline_id + '-interpolations">')
  interpolation_values = " ".join([str(x) for x in interps])
  self.writel(S_GEOM, 4, '<Name_array id="' + spline_id + '-interpolations-array" count="' +
     str(len(interps)) + '">' + interpolation_values + '</Name_array>')
  self.writel(S_GEOM, 4, '<technique_common>')
  self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(spline_id) +
     '-interpolations-array" count="' + str(len(interps)) + '" stride="1">')
  self.writel(S_GEOM, 5, '<param name="INTERPOLATION" type="name"/>')
  self.writel(S_GEOM, 4, '</accessor>')
  self.writel(S_GEOM, 4, '</technique_common>')
  self.writel(S_GEOM, 3, '</source>')
  self.writel(S_GEOM, 3, '<source id="' + spline_id + '-tilts">')
  tilt_values = " ".join([strflt(x) for x in tilts])
  self.writel(S_GEOM, 4, '<float_array id="' + spline_id + '-tilts-array" count="' +
     str(len(tilts)) + '">' + tilt_values + '</float_array>')
  self.writel(S_GEOM, 4, '<technique_common>')
  self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(spline_id) +
     '-tilts-array" count="' + str(len(tilts)) + '" stride="1">')
  self.writel(S_GEOM, 5, '<param name="TILT" type="float"/>')
  self.writel(S_GEOM, 4, '</accessor>')
  self.writel(S_GEOM, 4, '</technique_common>')
  self.writel(S_GEOM, 3, '</source>')
  self.writel(S_GEOM, 3, '<control_vertices>')
  self.writel(S_GEOM, 4, '<input semantic="POSITION" source="' +
     self.ref_id(spline_id) + '-positions"/>')
  self.writel(S_GEOM, 4, '<input semantic="IN_TANGENT" source="' +
     self.ref_id(spline_id) + '-intangents"/>')
  self.writel(S_GEOM, 4, '<input semantic="OUT_TANGENT" source="' +
     self.ref_id(spline_id) + '-outtangents"/>')
  self.writel(S_GEOM, 4, '<input semantic="INTERPOLATION" source="' +
     self.ref_id(spline_id) + '-interpolations"/>')
  self.writel(S_GEOM, 4, '<input semantic="TILT" source="' +
     self.ref_id(spline_id) + '-tilts"/>')
  self.writel(S_GEOM, 3, '</control_vertices>')
  self.writel(S_GEOM, 2, '</spline>')
  self.writel(S_GEOM, 1, '</geometry>')

 def get_bone_transform_xml(self, bone):
  return self.transform_to_xml(self.get_bone_transform(bone))

 def get_node_transform_xml(self, node):
  transform, visible = self.get_node_local_transform(node)
  return self.transform_to_xml(transform)

 def transform_to_xml(self, transform):
  if (self.transform_matrix_scale):
   transforms = [
    self.get_matrix_transform_xml(transform["matrix"]),
    self.get_scale_xml(transform["scale"])]
  else:
   transforms = [
    self.get_matrix_transform_xml(transform["matrix"])]

  return [e for t in transforms for e in t]

 def get_node_local_transform(self, node):
  visible = True
  if (node.parent_type == 'BONE'):
   armature = node.parent
   parent_bone = armature.data.bones[node.parent_bone]
   parent = parent_bone
   if (not parent):
    matrix = node.matrix_local.copy()
   else:
    pose_parent = armature.pose.bones[parent.name]
    if self.is_zero_scale(pose_parent.matrix) or self.is_zero_scale(armature.matrix_world):
     visible = False
     matrix = Matrix()
    else:
     matrix = pose_parent.matrix.inverted() * (armature.matrix_world.inverted()
                 * node.matrix_world)
  else:
   matrix = node.matrix_local.copy()
   if node.parent:
    if self.is_zero_scale(node.parent.matrix_local):
     visible = False

  if (node.type == 'CAMERA'):
   m = Matrix.Rotation(-math.pi / 2.0, 4, Vector((1, 0, 0)))
   matrix = matrix * m

  if (self.transform_matrix_scale):
   matrix, scale = self.split_matrix_scale(matrix)
   return {"matrix": matrix, "scale": scale}, visible
  else:
   return {"matrix": matrix}, visible

 def get_bone_deform_parent(self, bone):
  return bone.parent

 def split_matrix_scale(self, matrix):
  m = matrix.normalized()
  m.translation = matrix.to_translation()
  return m, matrix.to_scale()

 def get_bone_transform(self, bone):
  # get the transform relative to the parent bone.

  parent = self.get_bone_deform_parent(bone)
  if (parent != None) and not self.is_zero_scale(parent.matrix_local):
   matrix = parent.matrix_local.inverted() * bone.matrix_local
  else:
   matrix = bone.matrix_local.copy()
  # Bones are only scaled in pose mode
  if (self.transform_matrix_scale):
   return {"matrix": matrix, "scale": (1.0, 1.0, 1.0)}
  else:
   return {"matrix": matrix}

 def get_posebone_transform(self, posebones_map, posebone):
  # get posebone transform relative to its parent bone, if no parent bone
  # then relative to the armature
  matrix = posebone.matrix.copy()
  if (posebone.bone.parent):
   parent_bone = self.get_bone_deform_parent(posebone.bone)
   if (parent_bone):
    parent = posebones_map[parent_bone.name]
    if not self.is_zero_scale(parent.matrix):
     matrix = parent.matrix.inverted() * matrix
    else:
     return None

  if (self.transform_matrix_scale):
   matrix, scale = self.split_matrix_scale(matrix)
   return {"matrix": matrix, "scale": scale}
  else:
   return {"matrix": matrix}

 def get_scale_xml(self, scale):
  return ['<scale sid="scale">' + self.strxyz(scale, True) + '</scale>']

 def get_matrix_transform_xml(self, matrix):
  return['<matrix sid="transform">' + self.strmtx(matrix) + '</matrix>']

 def export_node(self, section, node, il, lookup, recurse=True):
  # export a scene node as a Collada node
  nodes_lookup = lookup["nodes"]

  prev_id = nodes_lookup.get(node, None)
  if prev_id:
   # previously exported node is occurring again
   node_id = prev_id
   instance_prev_node = True
  else:
   node_id = self.get_node_id(node.name)
   nodes_lookup[node] = node_id
   instance_prev_node = False

  instance_node=False
  
  if instance_prev_node:
   self.writel(section, il, '<instance_node url="{}">'.format(self.ref_id(node_id)))
   instance_node=True
   il += 1
  else:

   self.writel(section, il, '<node id="{}" name="{}" type="NODE">'.format(
    node_id, node.name))
   il += 1
 
   transforms = self.get_node_transform_xml(node)
   for t in transforms:
    self.writel(section, il, t)
 
   if (node.type == "ARMATURE"):
    self.export_armature_node(section, node, il, lookup, recurse)
   elif (node in lookup["node_to_skin"]):
    count = 0
    for skin_lookup in lookup["node_to_skin"][node]:
     skin_id = skin_lookup['skin']
     skeleton = skin_lookup['skeleton']
     skin_sid = "skin" + str(count)
     self.writel(section, il, '<instance_controller url="' +
        self.ref_id(skin_id) + '" sid="' + skin_sid + '">')
     self.writel(section, il + 1, '<skeleton>#' +
        skeleton + '</skeleton>')
     self.export_material_bind(section, node, il, lookup)
     self.writel(section, il, "</instance_controller>")
     count += 1
   elif (node.data in lookup["mesh_morphs"]):
    morph_id = lookup["mesh_to_morph_controller"][node.data][0]
    morph_sid = "morph"
    self.writel(section, il, '<instance_controller url="' +
       self.ref_id(morph_id) + '" sid="' + morph_sid + '">')
    self.export_material_bind(section, node, il, lookup)
    self.writel(section, il, "</instance_controller>")
   elif (node in lookup["node_to_mesh"]):
    mesh_id = lookup["node_to_mesh"][node]["id"]
    self.writel(
     section, il, '<instance_geometry url="' + self.ref_id(mesh_id) + '">')
    self.export_material_bind(section, node, il, lookup)
    self.writel(section, il, "</instance_geometry>")
   elif (node.data in lookup["camera"]):
    camera_id = lookup["camera"][node.data]
    self.writel(
     section, il, '<instance_camera url="' + self.ref_id(camera_id) + '"/>')
   elif (node.data in lookup["light"]):
    light_id = lookup["light"][node.data]
    self.writel(
     section, il, '<instance_light url="' + self.ref_id(light_id) + '"/>')
 
   if node.dupli_group and node.dupli_type == "GROUP":
    self.writel(
     section, il, '<instance_node url="{}"/>'.format(self.ref_library(node.dupli_group)))

  if recurse:
   for x in [child for child in node.children if child.parent_type!="BONE"]:
    self.export_node(section, x, il, lookup)

  il -= 1
  if not instance_node:
   self.writel(section, il, '</node>')
  else:
   self.writel(section, il, '</instance_node>')
   

 def ref_library(self, obj):
  id = obj.name
  if obj.library:
   path = self.library_to_xml_url(obj.library)
  else:
   path = ""
  return self.ref_id(path + "#" + id)

 def library_to_xml_url(self, library):
  return "{}.dae".format(os.path.join("./", os.path.splitext(urlparse(library.filepath).netloc)[0].replace("\\", "/")))

 def get_armature_children_of_bones(self, armature_node):
  # get the scene nodes that are attached to the armature through bone
  # parenting

  def get_children_of_bones(children, parenting_map):
   if (children == None):
    return
   for child in children:
    if (hasattr(child, "parent_bone") and (child.parent_bone != None)):
     if (not child.parent_bone in parenting_map):
      parenting_map[child.parent_bone] = []
     parenting_map[child.parent_bone].append(child)
    get_children_of_bones(child.children, parenting_map)

  parenting_map = {}
  get_children_of_bones(armature_node.children, parenting_map)
  return parenting_map

 def is_node_valid(self, node):
  if node == None:
   return False
  if (not hasattr(node, "data")):
   return False
  if (not node.data):
   return False
#  if (not node.type in self.config["object_types"]):
#   return False
  if (self.config["use_export_selected"] and not node.select):
   return False
  if (self.use_active_layers):
   valid = False
   for i in range(20):
    if (node.layers[i] and self.bpy_context_scene.layers[i]):
     valid = True
     break
   if (not valid):
    return False
  return True

 def export_material_bind(self, section, node, il, lookup):
  if not node.material_slots:
   return
  if not len([slot.material for slot in node.material_slots if slot.material]):
   return
  material_bind = lookup["node_to_mesh"][node].get("material_bind", None)
  if not material_bind:
   return

  self.writel(section, il + 1, '<bind_material>')
  self.writel(section, il + 2, '<technique_common>')
  for material_slot, material_symbol in material_bind.items():
   material = node.material_slots[material_slot].material
   if material:
    if not material.library:
     material_id = lookup["material"][material]
    else:
     material_id = self.ref_library(material)
    self.writel(section, il + 3, '<instance_material symbol="{}" target="{}">'.format(
     material_symbol, self.ref_id(material_id)))
    self.writel(
     section, il + 4, '<bind_vertex_input semantic="UVMap" input_semantic="TEXCOORD" input_set="0"/>')
    self.writel(section, il + 3, '</instance_material>')
  self.writel(section, il + 2, '</technique_common>')
  self.writel(section, il + 1, '</bind_material>')

 def export_materials(self, lookup):

  material_lookup = lookup["material"]
  effect_lookup = lookup["effect"]
  image_lookup = lookup["image"]

  # get materials for export
  materials = {slot.material for n in self.bpy_context_scene.objects if hasattr(
   n, "material_slots") and n in self.visual_nodes for slot in n.material_slots if slot.material and not slot.material.library if slot.material not in material_lookup}

  # get texture map images

  # Textures based selected materials
  images = {slot.texture.image for m in materials if not m.library for slot in m.texture_slots if slot and slot.use and slot.texture and slot.texture.type == "IMAGE" and slot.texture.image if slot.texture.image not in image_lookup
      }.union({node.image for m in materials if not m.library and m.node_tree for node in m.node_tree.nodes if node.type == "TEX_IMAGE" if node.image not in image_lookup})

  # export library_images content

  for image in images:
   image_id = self.get_node_id(image.name + "-image")
   image_lookup[image] = image_id
   self.export_image(image, image_id)

  # export library_effects content

  for mat in materials:
   effect_id = self.get_node_id(mat.name + "-effect")
   effect_lookup[mat] = effect_id
   self.export_effect(mat, effect_id, image_lookup)

  # export library_materials content
  for mat in materials:
   material_id = self.get_node_id(mat.name + "-material")
   self.export_material(mat, material_id, effect_lookup[mat])
   material_lookup[mat] = material_id

 def external_ref(self, library, id):
  return "./" + os.path.splitext(urlparse(library.filepath).netloc)[0].replace("\\", "/") + ".dae" + self.ref_id(id)

 def export_material(self, material, material_id, effect_id):
  # Material
  self.writel(S_MATS, 1, '<material id="' + material_id +
     '" name="' + material.name + '">')
  self.writel(S_MATS, 2, '<instance_effect url="' +
     self.ref_id(effect_id) + '"/>')
  self.writel(S_MATS, 1, '</material>')

 def get_visual_nodes(self):
    # validate nodes
  for obj in self.bpy_context_scene.objects:
   if (obj in self.visual_nodes):
    continue
   if (self.is_node_valid(obj)):
    n = obj
    while (n != None):
     if (not n in self.visual_nodes):
      self.visual_nodes.add(n)
     n = n.parent

 def get_groups(self):
  groups = {group for group in bpy.data.groups if not group.library}
  visual_groups = {group for group in groups if next(
   (object for object in group.objects if object in self.visual_nodes), None)}
  instanced_groups = {node.dupli_group for node in self.bpy_context_scene.objects if (
   node.dupli_group and node.dupli_type == "GROUP" and node.dupli_group in groups) and node in self.visual_nodes}
  return visual_groups.union(instanced_groups)

 def export_game_physics_nodes(self, physics_nodes, lookup):
  self.export_game_physics_materials(physics_nodes, lookup)
  self.export_game_rigid_body_models(physics_nodes, lookup)
  self.export_game_physics_scene(physics_nodes, lookup)

 def export_game_physics_materials(self, physics_nodes, lookup):
  physics_materials_lookup = lookup["physics_material"]
  for node in physics_nodes:
   if (not node.game in physics_materials_lookup):
    physics_material_id = self.get_node_id(node.name + '-phys_mat')
    physics_materials_lookup[node.game] = physics_material_id
    self.export_game_physics_material(node.game, physics_material_id)

 def export_game_physics_material(self, node_game, physics_material_id):
  self.writel(S_P_MATS, 1, '<physics_material id ="{}">'.format(
   physics_material_id))
#  self.writel(S_P_MATS, 2, '<technique_common>')
#  self.writel(
#   S_P_MATS, 3, '<dynamic_friction>{}</dynamic_friction>'.format(0))
#  self.writel(
#   S_P_MATS, 3, '<static_friction>{}</static_friction>'.format(0))
#  self.writel(S_P_MATS, 3, '<restitution>{}</restitution>'.format(0))
#  self.writel(S_P_MATS, 2, '</technique_common>')
  self.writel(S_P_MATS, 2, '<extra>')
  if node_game.use_anisotropic_friction:
   self.writel(S_P_MATS, 3, '<technique profile="bullet">')
   self.writel(S_P_MATS, 4, '<anisotropic_friction>{}</anisotropic_friction>'.format(
    self.strxyz(node_game.friction_coefficients)))
   self.writel(S_P_MATS, 3, '</technique>')
  if (node_game.physics_type == 'DYNAMIC' or node_game.physics_type == 'RIGID_BODY') and node_game.use_rotate_from_normal:
   self.writel(S_P_MATS, 3, '<technique profile="blender">')
   self.writel(
    S_P_MATS, 4, '<use_rotate_from_normal>true</use_rotate_from_normal>')
   self.writel(S_P_MATS, 3, '</technique>')
  self.writel(S_P_MATS, 2, '</extra>')
  self.writel(S_P_MATS, 1, '</physics_material>')

 def export_game_rigid_body_models(self, physics_nodes, lookup):
  physics_materials_lookup = lookup["physics_material"]
  physics_rigid_body_lookup = lookup["physics_rigid_body"]
  for node in physics_nodes:
   if node not in physics_rigid_body_lookup:
    physics_model_id = self.get_node_id(node.name + '-model')
    physics_body_sid = self.get_node_id(node.name + '-body')
    physics_rigid_body_lookup[node.game] = {
     'model_id': physics_model_id, 'body_sid': physics_body_sid}
    self.export_game_rigid_body_model(
     node, physics_model_id, physics_body_sid, lookup)

 def export_game_rigid_body_model(self, node, physics_model_id, physics_body_sid, lookup):
  self.writel(
   S_P_MODEL, 1, '<physics_model id="{}">'.format(physics_model_id))
  self.writel(
   S_P_MODEL, 2, '<rigid_body sid="{}">'.format(physics_body_sid))
  self.writel(S_P_MODEL, 3, '<technique_common>')
  active_types = ['DYNAMIC', 'RIGID_BODY']
  self.writel(S_P_MODEL, 4, '<dynamic>{}</dynamic>'.format(
   str(node.game.physics_type in active_types).lower()))
  self.writel(S_P_MODEL, 4, '<mass>{}</mass>'.format(node.game.mass))
  self.writel(S_P_MODEL, 4, '<mass_frame>')
  self.writel(S_P_MODEL, 5, '<translate>0 0 0</translate>')
  self.writel(S_P_MODEL, 5, '<rotate>0 0 1 0</rotate>')
  self.writel(S_P_MODEL, 4, '</mass_frame>')
  self.writel(S_P_MODEL, 4, '<shape>')
  try:  # in case no physics material in the lookup
   self.writel(S_P_MODEL, 5, '<instance_physics_material url="{}"/>'.format(
    self.ref_id(lookup['physics_material'][node.game])))
  except:
   pass  # don't need it then
  if node.game.use_collision_bounds:
   shape = node.game.collision_bounds_type
  else:
   shape = 'TRIANGLE_MESH'
  try:
   self.shape_funcs[shape](node, 5, lookup)
  except:
   pass
  if node.game.use_collision_bounds:
   self.export_game_collision_margin(node, 5)

  self.writel(S_P_MODEL, 4, '</shape>')
  self.writel(S_P_MODEL, 3, '</technique_common>')

  self.writel(S_P_MODEL, 3, '<extra>')
  self.writel(S_P_MODEL, 4, '<technique profile="bullet">')
  self.writel(
   S_P_MODEL, 5, '<linear_damping>{}</linear_damping>'.format(node.game.damping))
  self.writel(
   S_P_MODEL, 5, '<angular_damping>{}</angular_damping>'.format(node.game.rotation_damping))
  self.writel(
   S_P_MODEL, 5, '<deactivation use="{}"/>'.format(str(node.game.use_sleep).lower()))
  collision_groups = [str(i) for (i, g) in enumerate(
   node.game.collision_group) if g]
  collision_mask = [str(i) for (i, g) in enumerate(
  node.game.collision_mask) if g]
  self.writel(
   S_P_MODEL, 5, '<collision_filter_groups>{}</collision_filter_groups>'.format(" ".join(collision_groups)))
  self.writel(
   S_P_MODEL, 5, '<collision_filter_mask>{}</collision_filter_mask>'.format(" ".join(collision_mask)))
  linear_factor = [1.0, 1.0, 1.0]
  angular_factor = [1.0, 1.0, 1.0]
  if node.game.lock_location_x:
   linear_factor[0] = 0.0
  if node.game.lock_location_y:
   linear_factor[1] = 0.0
  if node.game.lock_location_z:
   linear_factor[2] = 0.0
  if node.game.lock_rotation_x:
   angular_factor[0] = 0.0
  if node.game.lock_rotation_y:
   angular_factor[1] = 0.0
  if node.game.lock_rotation_z:
   angular_factor[2] = 0.0
  self.writel(
   S_P_MODEL, 5, '<linear_factor>{}</linear_factor>'.format(self.strxyz(linear_factor, True)))
  self.writel(
   S_P_MODEL, 5, '<angular_factor>{}</angular_factor>'.format(self.strxyz(angular_factor, True)))
  self.writel(S_P_MODEL, 4, '</technique>')
  self.writel(S_P_MODEL, 4, '<technique profile="blender">')
  self.writel(
   S_P_MODEL, 5, '<physics_type>{}</physics_type>'.format(node.game.physics_type))
  self.writel(
   S_P_MODEL, 5, '<use_actor>{}</use_actor>'.format(str(node.game.use_actor).lower()))
  self.writel(
   S_P_MODEL, 5, '<use_ghost>{}</use_ghost>'.format(str(node.game.use_ghost).lower()))
  self.writel(S_P_MODEL, 4, '</technique>')
  self.writel(S_P_MODEL, 3, '</extra>')
  self.writel(S_P_MODEL, 2, '</rigid_body>')
  self.writel(S_P_MODEL, 1, '</physics_model>')

 def export_game_collision_margin(self, node, il):
  self.writel(S_P_MODEL, il, '<extra>')
  self.writel(S_P_MODEL, il + 1, '<technique profile="bullet">')
  self.writel(
   S_P_MODEL, il + 2,
   '<collision_margin>{}</collision_margin>'
   .format(node.game.collision_margin)
  )
  self.writel(S_P_MODEL, il + 1, '</technique>')
  self.writel(S_P_MODEL, il, '</extra>')

 def export_game_physics_scene(self, physics_nodes, lookup):

  self.writel(S_P_SCENE, 1, '<physics_scene id="{}">'.format(
   self.get_scene_name(self.bpy_context_scene) + '-physics'))
  self.writel(S_P_SCENE, 2, '<technique_common>')
  self.writel(
   S_P_SCENE, 3, '<gravity>0 0 {}</gravity>'.format(self.bpy_context_scene.game_settings.physics_gravity))
  self.writel(
   S_P_SCENE, 3, '<time_step>{}</time_step>'.format(1.0 / self.bpy_context_scene.game_settings.fps))
  self.writel(S_P_SCENE, 2, '</technique_common>')
  self.writel(S_P_SCENE, 2, '<extra>')
  self.writel(S_P_SCENE, 3, '<technique profile="bullet">')
  self.writel(S_P_SCENE, 4, '<max_steps>{}</max_steps>'.format(
   self.bpy_context_scene.game_settings.physics_step_max))
  self.writel(S_P_SCENE, 4, '<sub_steps>{}</sub_steps>'.format(
   self.bpy_context_scene.game_settings.physics_step_sub))
  self.writel(S_P_SCENE, 4, '<deactivation use="true">')
  self.writel(S_P_SCENE, 5, '<linear_velocity>{}</linear_velocity>'.format(
   self.bpy_context_scene.game_settings.deactivation_linear_threshold))
  self.writel(S_P_SCENE, 5, '<angular_velocity>{}</angular_velocity>'.format(
   self.bpy_context_scene.game_settings.deactivation_angular_threshold))
  self.writel(S_P_SCENE, 5, '<time>{}</time>'.format(
   strflt(self.bpy_context_scene.game_settings.deactivation_time)))
  self.writel(S_P_SCENE, 4, '</deactivation>')
  self.writel(S_P_SCENE, 3, '</technique>')
  self.writel(S_P_SCENE, 2, '</extra>')
  self.export_game_physics_instances(physics_nodes, lookup)
  self.writel(S_P_SCENE, 1, '</physics_scene>')

 def export_game_physics_instances(self, physics_nodes, lookup):
  physics_rigid_body_lookup = lookup["physics_rigid_body"]
  for node in physics_nodes:
   physics_model = physics_rigid_body_lookup[node.game]
   sid = self.get_node_id(node.name + '-' + physics_model['model_id'])
   url = physics_model['model_id']
   parent = lookup['nodes'][node]
   self.writel(S_P_SCENE, 2, '<instance_physics_model sid="{}" url="{}" parent="{}">'.format(
    sid, self.ref_id(url), self.ref_id(parent)))
   rigid_body = physics_model['body_sid']
   self.writel(S_P_SCENE, 3, '<instance_rigid_body body="{}" target="{}">'.format(
    rigid_body, self.ref_id(parent)))
   self.writel(S_P_SCENE, 4, '<technique_common>')
   # self.writel(S_P_SCENE, 5, '<angular_velocity>0 0 0</angular_velocity>')
   # self.writel(S_P_SCENE, 5, '<velocity>0 0 0</velocity>')
   self.writel(S_P_SCENE, 5, '<mass>{}</mass>'.format(node.game.mass))
   self.writel(S_P_SCENE, 4, '</technique_common>')
   self.writel(S_P_SCENE, 3, '</instance_rigid_body>')
   self.writel(S_P_SCENE, 2, '</instance_physics_model>')

 def export_physics_nodes(self, physics_nodes, lookup):
  self.export_physics_materials(physics_nodes, lookup)
  self.export_physics_rigid_body_models(physics_nodes, lookup)
  self.export_physics_scene(physics_nodes, lookup)

 def export_physics_materials(self, physics_nodes, lookup):
  physics_materials_lookup = lookup["physics_material"]
  for node in physics_nodes:
   if (not node.data in physics_materials_lookup):
    physics_material_id = self.get_node_id(
     node.data.name + '-phys_mat')
    physics_materials_lookup[node.data] = physics_material_id
    self.export_physics_material(node, physics_material_id)

 def export_physics_material(self, node, physics_material_id):
  self.writel(S_P_MATS, 1, '<physics_material id ="{}">'.format(
   physics_material_id))
  self.writel(S_P_MATS, 2, '<technique_common>')
  self.writel(
   S_P_MATS, 3, '<dynamic_friction>{}</dynamic_friction>'.format(node.rigid_body.friction))
  self.writel(
   S_P_MATS, 3, '<static_friction>{}</static_friction>'.format(node.rigid_body.friction))
  self.writel(
   S_P_MATS, 3, '<restitution>{}</restitution>'.format(node.rigid_body.restitution))
  self.writel(S_P_MATS, 2, '</technique_common>')
  self.writel(S_P_MATS, 1, '</physics_material>')

 def export_physics_rigid_body_models(self, physics_nodes, lookup):
  physics_materials_lookup = lookup["physics_material"]
  physics_rigid_body_lookup = lookup["physics_rigid_body"]
  for node in physics_nodes:
   if (not node.data in physics_rigid_body_lookup):
    physics_model_id = self.get_node_id(node.data.name + '-model')
    physics_body_sid = self.get_node_id(node.data.name + '-body')
    physics_rigid_body_lookup[node.data] = {
     'model_id': physics_model_id, 'body_sid': physics_body_sid}
    self.export_physics_rigid_body_model(
     node, physics_model_id, physics_body_sid, lookup)
  return physics_rigid_body_lookup

 def export_physics_rigid_body_model(self, node, physics_model_id, physics_body_sid, lookup):
  self.writel(
   S_P_MODEL, 1, '<physics_model id="{}">'.format(physics_model_id))
  self.writel(
   S_P_MODEL, 2, '<rigid_body sid="{}">'.format(physics_body_sid))
  self.writel(S_P_MODEL, 3, '<technique_common>')
  self.writel(S_P_MODEL, 4, '<dynamic>{}</dynamic>'.format(
   str(node.rigid_body.type == 'ACTIVE').lower()))
  self.writel(
   S_P_MODEL, 4, '<mass>{}</mass>'.format(node.rigid_body.mass))
  self.writel(S_P_MODEL, 4, '<mass_frame>')
  self.writel(S_P_MODEL, 5, '<translate>0 0 0</translate>')
  self.writel(S_P_MODEL, 5, '<rotate>0 0 1 0</rotate>')
  self.writel(S_P_MODEL, 4, '</mass_frame>')
  self.writel(S_P_MODEL, 4, '<shape>')
  try:  # in case no physics material in the lookup
   self.writel(S_P_MODEL, 5, '<instance_physics_material url="{}"/>'.format(
    self.ref_id(lookup['physics_material'][node.data])))
  except:
   pass  # don't need it then
  try:
   self.shape_funcs[node.rigid_body.collision_shape](node, 5, lookup)
  except:
   pass
  self.export_collision_margin(node, 5)

  self.writel(S_P_MODEL, 4, '</shape>')
  self.writel(S_P_MODEL, 3, '</technique_common>')

  self.writel(S_P_MODEL, 3, '<extra>')
  self.writel(S_P_MODEL, 4, '<technique profile="bullet">')
  self.writel(
   S_P_MODEL, 5, '<linear_damping>{}</linear_damping>'.format(node.rigid_body.linear_damping))
  self.writel(
   S_P_MODEL, 5, '<angular_damping>{}</angular_damping>'.format(node.rigid_body.angular_damping))
  if(node.rigid_body.use_deactivation):
   self.writel(S_P_MODEL, 5, '<deactivation use="true">')
   self.writel(S_P_MODEL, 6, '<linear_velocity>{}</linear_velocity>'.format(
    node.rigid_body.deactivate_linear_velocity))
   self.writel(S_P_MODEL, 6, '<angular_velocity>{}</angular_velocity>'.format(
    node.rigid_body.deactivate_angular_velocity))
   self.writel(S_P_MODEL, 6, '<start>{}</start>'.format(
    str(node.rigid_body.use_start_deactivated).lower()))
   self.writel(S_P_MODEL, 5, '</deactivation>')
  collision_groups = [str(i) for (i, g) in enumerate(
   node.rigid_body.collision_groups) if g]
  if (len(collision_groups)):
   self.writel(
    S_P_MODEL, 5, '<collision_filter_groups>{}</collision_filter_groups>'.format(" ".join(collision_groups)))
  linear_factor = [1.0, 1.0, 1.0]
  angular_factor = [1.0, 1.0, 1.0]
  self.writel(
   S_P_MODEL, 5, '<linear_factor>{}</linear_factor>'.format(self.strxyz(linear_factor, True)))
  self.writel(
   S_P_MODEL, 5, '<angular_factor>{}</angular_factor>'.format(self.strxyz(angular_factor, True)))
  self.writel(S_P_MODEL, 4, '</technique>')
  self.writel(S_P_MODEL, 3, '</extra>')
  self.writel(S_P_MODEL, 2, '</rigid_body>')
  self.writel(S_P_MODEL, 1, '</physics_model>')

 def export_physics_scene(self, physics_nodes, lookup):
  if (self.bpy_context_scene.rigidbody_world == None):
   return

  self.writel(S_P_SCENE, 1, '<physics_scene id="{}">'.format(
   self.get_scene_name(self.bpy_context_scene) + '-physics'))
  self.writel(S_P_SCENE, 2, '<technique_common>')
  self.writel(
   S_P_SCENE, 3, '<gravity>{}</gravity>'.format(self.strxyz(self.bpy_context_scene.gravity)))
  self.writel(S_P_SCENE, 3, '<time_step>{}</time_step>'.format(1.0 /
                  self.bpy_context_scene.rigidbody_world.steps_per_second))
  self.writel(S_P_SCENE, 2, '</technique_common>')
  self.writel(S_P_SCENE, 2, '<extra>')
  self.writel(S_P_SCENE, 3, '<technique profile="bullet">')
  self.writel(S_P_SCENE, 4, '<time_scale>{}</time_scale>'.format(
   self.bpy_context_scene.rigidbody_world.time_scale))
  self.writel(S_P_SCENE, 4, '<solver_iterations>{}</solver_iterations>'.format(
   self.bpy_context_scene.rigidbody_world.solver_iterations))
  self.writel(S_P_SCENE, 4, '<split_impulse>{}</split_impulse>'.format(
   str(self.bpy_context_scene.rigidbody_world.use_split_impulse).lower()))
  self.writel(S_P_SCENE, 3, '</technique>')
  self.writel(S_P_SCENE, 3, '<technique profile="blender">')
  self.writel(S_P_SCENE, 4, '<weights>')
  self.writel(S_P_SCENE, 5, '<all>{}</all>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.all))
  self.writel(S_P_SCENE, 5, '<gravity>{}</gravity>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.gravity))
  self.writel(S_P_SCENE, 5, '<force>{}</force>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.force))
  self.writel(S_P_SCENE, 5, '<harmonic>{}</harmonic>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.harmonic))
  self.writel(S_P_SCENE, 5, '<vortex>{}</vortex>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.vortex))
  self.writel(S_P_SCENE, 5, '<charge>{}</charge>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.charge))
  self.writel(S_P_SCENE, 5, '<magnetic>{}</magnetic>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.magnetic))
  self.writel(S_P_SCENE, 5, '<lennardjones>{}</lennardjones>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.lennardjones))
  self.writel(S_P_SCENE, 5, '<wind>{}</wind>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.wind))
  self.writel(S_P_SCENE, 5, '<turbulence>{}</turbulence>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.turbulence))
  self.writel(S_P_SCENE, 5, '<curve_guide>{}</curve_guide>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.curve_guide))
  self.writel(S_P_SCENE, 5, '<drag>{}</drag>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.drag))
  self.writel(S_P_SCENE, 5, '<texture>{}</texture>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.texture))
  self.writel(S_P_SCENE, 5, '<boid>{}</boid>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.boid))
  self.writel(S_P_SCENE, 5, '<smokeflow>{}</smokeflow>'.format(
   self.bpy_context_scene.rigidbody_world.effector_weights.smokeflow))
  self.writel(S_P_SCENE, 4, '</weights>')
  self.writel(S_P_SCENE, 3, '</technique>')
  self.writel(S_P_SCENE, 2, '</extra>')
  self.export_physics_instances(physics_nodes, lookup)
  self.writel(S_P_SCENE, 1, '</physics_scene>')

 def export_physics_instances(self, physics_nodes, lookup):
  for node in physics_nodes:
   physics_model = lookup['physics_rigid_body'][node.data]
   sid = self.get_node_id(node.name + '-' + physics_model['model_id'])
   url = physics_model['model_id']
   parent = lookup['nodes'][node]
   self.writel(S_P_SCENE, 2, '<instance_physics_model sid="{}" url="{}" parent="{}">'.format(
    sid, self.ref_id(url), self.ref_id(parent)))
   rigid_body = physics_model['body_sid']
   self.writel(S_P_SCENE, 3, '<instance_rigid_body body="{}" target="{}">'.format(
    rigid_body, self.ref_id(parent)))
   self.writel(S_P_SCENE, 4, '<technique_common>')
   # self.writel(S_P_SCENE, 5, '<angular_velocity>0 0 0</angular_velocity>')
   # self.writel(S_P_SCENE, 5, '<velocity>0 0 0</velocity>')
   self.writel(
    S_P_SCENE, 5, '<mass>{}</mass>'.format(node.rigid_body.mass))
   self.writel(S_P_SCENE, 4, '</technique_common>')
   self.writel(S_P_SCENE, 3, '</instance_rigid_body>')
   self.writel(S_P_SCENE, 2, '</instance_physics_model>')

 def get_node_dimensions(self, node):
  # get dimensions before scaling
  return [d / s for (d, s)in zip(node.dimensions, node.scale)]

 def export_collision_margin(self, node, il):
  if (node.rigid_body.use_margin or node.rigid_body.collision_shape == 'CONE'):
   self.writel(S_P_MODEL, il, '<extra>')
   self.writel(S_P_MODEL, il + 1, '<technique profile="bullet">')
   self.writel(
    S_P_MODEL, il + 2,
    '<collision_margin>{}</collision_margin>'
    .format(node.rigid_body.collision_margin))
   self.writel(S_P_MODEL, il + 1, '</technique>')
   self.writel(S_P_MODEL, il, '</extra>')

 def export_box_shape(self, node, il, lookup):
  dimensions = self.get_node_dimensions(node)
  self.writel(S_P_MODEL, il, "<box>")
  self.writel(S_P_MODEL, il + 1, "<half_extents>{}</half_extents>".format(
   self.strxyz([dimensions[0] / 2.0, dimensions[1] / 2.0, dimensions[2] / 2.0])))
  self.writel(S_P_MODEL, il, "</box>")

 def export_sphere_shape(self, node, il, lookup):
  dimensions = self.get_node_dimensions(node)
  self.writel(S_P_MODEL, il, "<sphere>")
  radius = max(dimensions) / 2.0
  self.writel(S_P_MODEL, il + 1, "<radius>{}</radius>".format(radius))
  self.writel(S_P_MODEL, il, "</sphere>")

 def export_capsule_shape(self, node, il, lookup):
  dimensions = self.get_node_dimensions(node)
  self.writel(S_P_MODEL, il, "<capsule>")
  self.writel(S_P_MODEL, il + 1,
     "<height>{}</height>".format(dimensions[2]))
  radius = max(dimensions[0], dimensions[1]) / 2.0
  self.writel(S_P_MODEL, il + 1,
     "<radius>{} {} {}</radius>".format(radius, radius, radius))
  self.writel(S_P_MODEL, il, "</capsule>")

 def export_cylinder_shape(self, node, il, lookup):
  dimensions = self.get_node_dimensions(node)
  self.writel(S_P_MODEL, il, "<cylinder>")
  self.writel(S_P_MODEL, il + 1,
     "<height>{}</height>".format(dimensions[2]))
  radius = max(dimensions[0], dimensions[1]) / 2.0
  self.writel(S_P_MODEL, il + 1,
     "<radius>{} {}</radius>".format(radius, radius))
  self.writel(S_P_MODEL, il, "</cylinder>")

 def export_cone_shape(self, node, il, lookup):
  dimensions = self.get_node_dimensions(node)
  self.writel(S_P_MODEL, il, "<cone>")
  self.writel(S_P_MODEL, il + 1,
     "<height>{}</height>".format(dimensions[2]))
  radius = max(dimensions[0], dimensions[1]) / 2.0
  self.writel(S_P_MODEL, il + 1,
     "<radius>{} {}</radius>".format(radius, radius))
  self.writel(S_P_MODEL, il, "</cone>")

 def get_physics_mesh_id(self, node, lookup):

  # get the source for building a physics mesh based on the mesh_source
  # setting. Return True, mesh if the mesh is convex otherwise False, mesh
  try:
   mesh = lookup['convex_mesh'].get(node.data, None)
   if not mesh:
    mesh = lookup["mesh"][node.data]
    return False, mesh
   else:
    return True, mesh
  except:
   return False, None

 def export_convex_hull_shape(self, node, il, lookup):
  convex, mesh_id = self.get_physics_mesh_id(node, lookup)
  if (mesh_id != None):
   if (convex):
    self.writel(S_P_MODEL, il,
       '<instance_geometry url="{}"/>'.format(self.ref_id(mesh_id)))
   else:
    self.writel(
     S_P_MODEL, il, '<convex_mesh convex_hull_of="{}"/>'.format(self.ref_id(mesh_id)))

 def export_mesh_shape(self, node, il, lookup):
  convex, mesh_id = self.get_physics_mesh_id(node, lookup)
  if (mesh_id != None):
   self.writel(S_P_MODEL, il,
      '<instance_geometry url="{}"/>'.format(self.ref_id(mesh_id)))

 def export_groups(self, lookup):

  groups = self.get_groups()

  for group in groups:
   # have to use specified group name because it may be referenced from external file
   group_id = group.name
   self.writel(S_LIBRARY_NODES, 1, '<node id="{}" name="{}" sid="GROUP" type="NODE">'.format(
    group_id, group_id))
   for obj in group.objects:
    self.export_node(S_LIBRARY_NODES, obj, 2, lookup, False)
   self.writel(S_LIBRARY_NODES, 1, '</node>')

 def export_scene(self, lookup):

  self.writel(S_NODES, 1, '<visual_scene id="' +
     self.get_scene_name(self.bpy_context_scene) + '" name="' + self.get_scene_name(self.bpy_context_scene) + '">')

  for obj in self.bpy_context_scene.objects:
   if (obj.parent == None and obj in self.visual_nodes):
    self.export_node(S_NODES, obj, 2, lookup)

  self.writel(S_NODES, 1, '</visual_scene>')

 def export_asset(self):
  self.writel(S_ASSET, 0, '<asset>')
  self.writel(S_ASSET, 1, '<contributor>')
  self.writel(S_ASSET, 2, '<author> Anonymous </author>')
  self.writel(
   S_ASSET, 2, '<authoring_tool> Collada Exporter for Blender 2.6+, by Gregery Barton (gregery20@yahoo.com.au) </authoring_tool>')
  self.writel(S_ASSET, 1, '</contributor>')
  self.writel(S_ASSET, 1, '<created>' +
     time.strftime("%Y-%m-%dT%H:%M:%SZ  ") + '</created>')
  self.writel(S_ASSET, 1, '<modified>' +
     time.strftime("%Y-%m-%dT%H:%M:%SZ") + '</modified>')
  self.writel(S_ASSET, 1, '<unit meter="1.0" name="meter"/>')
  if self.axis_type == "ZUP":
   axis = "Z_UP"
  elif self.axis_type == "YUPR" or self.axis_type == "YUPL":
   axis = "Y_UP"
  self.writel(S_ASSET, 1, '<up_axis>' + axis + '</up_axis>')
  self.writel(S_ASSET, 0, '</asset>')

 def export_animation_blends(self, target, action_name, keys):

  frame_total = len(keys)
  if (action_name == None):
   anim_id = target + "-anim"
  else:
   anim_id = action_name + "-" + target + "-anim"
  anim_id = self.get_node_id(anim_id.replace("/", "-"))

  self.writel(S_ANIM, 1, '<animation id="' + anim_id + '">')

  source_frames = " ".join([strflt(k[0]) for k in keys])
  source_value = " ".join([strflt(k[1]) for k in keys])

  source_interps = " ".join([(" LINEAR ") * len(keys)])

  # Time Source
  self.writel(S_ANIM, 2, '<source id="' + anim_id + '-input">')
  self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-input-array" count="' +
     str(frame_total) + '">' + source_frames + '</float_array>')
  self.writel(S_ANIM, 3, '<technique_common>')
  self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(anim_id) +
     '-input-array" count="' + str(frame_total) + '" stride="1">')
  self.writel(S_ANIM, 5, '<param name="TIME" type="float"/>')
  self.writel(S_ANIM, 4, '</accessor>')
  self.writel(S_ANIM, 3, '</technique_common>')
  self.writel(S_ANIM, 2, '</source>')

  # Value Source
  self.writel(S_ANIM, 2, '<source id="' + anim_id + '-weights-output">')
  self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-weights-output-array" count="' +
     str(frame_total) + '">' + source_value + '</float_array>')
  self.writel(S_ANIM, 3, '<technique_common>')
  self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(anim_id) +
     '-weights-output-array" count="' + str(frame_total) + '" stride="1">')
  self.writel(S_ANIM, 5, '<param name="X" type="float"/>')
  self.writel(S_ANIM, 4, '</accessor>')
  self.writel(S_ANIM, 3, '</technique_common>')
  self.writel(S_ANIM, 2, '</source>')

  # Interpolation Source
  self.writel(S_ANIM, 2, '<source id="' +
     anim_id + '-interpolation-output">')
  self.writel(S_ANIM, 3, '<Name_array id="' + anim_id + '-interpolation-output-array" count="' +
     str(frame_total) + '">' + source_interps + '</Name_array>')
  self.writel(S_ANIM, 3, '<technique_common>')
  self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(anim_id) +
     '-interpolation-output-array" count="' + str(frame_total) + '" stride="1">')
  self.writel(S_ANIM, 5, '<param name="INTERPOLATION" type="Name"/>')
  self.writel(S_ANIM, 4, '</accessor>')
  self.writel(S_ANIM, 3, '</technique_common>')
  self.writel(S_ANIM, 2, '</source>')

  self.writel(S_ANIM, 2, '<sampler id="' +
     anim_id + '-weights-sampler">')
  self.writel(S_ANIM, 3, '<input semantic="INPUT" source="' +
     self.ref_id(anim_id) + '-input"/>')
  self.writel(S_ANIM, 3, '<input semantic="OUTPUT" source="' +
     self.ref_id(anim_id) + '-weights-output"/>')
  self.writel(S_ANIM, 3, '<input semantic="INTERPOLATION" source="' +
     self.ref_id(anim_id) + '-interpolation-output"/>')
  self.writel(S_ANIM, 2, '</sampler>')

  self.writel(S_ANIM, 2, '<channel source="' + self.ref_id(anim_id) +
     '-weights-sampler" target="' + target + '"/>')
  self.writel(S_ANIM, 1, '</animation>')

  return anim_id

 def export_animation_xforms(self, target, action_name, keys):

  frame_total = len(keys)
  if (action_name == None):
   anim_id = target + "-anim"
  else:
   anim_id = action_name + "-" + target + "-anim"

  if not self.multichannel_single_clip:
   self.writel(S_ANIM, 1, '<animation id="' + anim_id + '">')

  source_frames = " ".join([strflt(k[0]) for k in keys])
  source_matrix = " ".join([self.strmtx(k[1]['matrix']) for k in keys])
  if (self.transform_matrix_scale):
   source_scale = " ".join([self.strxyz(k[1]['scale']) for k in keys])

  source_interps = " ".join([(" LINEAR ") * len(keys)])

  # Time Source
  self.writel(S_ANIM, 2, '<source id="' + anim_id + '-input">')
  self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-input-array" count="' +
     str(frame_total) + '">' + source_frames + '</float_array>')
  self.writel(S_ANIM, 3, '<technique_common>')
  self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(anim_id) +
     '-input-array" count="' + str(frame_total) + '" stride="1">')
  self.writel(S_ANIM, 5, '<param name="TIME" type="float"/>')
  self.writel(S_ANIM, 4, '</accessor>')
  self.writel(S_ANIM, 3, '</technique_common>')
  self.writel(S_ANIM, 2, '</source>')

  # Transform Source
  self.writel(S_ANIM, 2, '<source id="' + anim_id + '-matrix-output">')
  self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-matrix-output-array" count="' +
     str(frame_total * 16) + '">' + source_matrix + '</float_array>')
  self.writel(S_ANIM, 3, '<technique_common>')
  self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(anim_id) +
     '-matrix-output-array" count="' + str(frame_total) + '" stride="16">')
  self.writel(S_ANIM, 5, '<param name="TRANSFORM" type="float4x4"/>')
  self.writel(S_ANIM, 4, '</accessor>')
  self.writel(S_ANIM, 3, '</technique_common>')
  self.writel(S_ANIM, 2, '</source>')

  # Scale Source
  if (self.transform_matrix_scale):
   self.writel(S_ANIM, 2, '<source id="' +
      anim_id + '-scale-output">')
   self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-scale-output-array" count="' +
      str(frame_total * 3) + '">' + source_scale + '</float_array>')
   self.writel(S_ANIM, 3, '<technique_common>')
   self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(anim_id) +
      '-scale-output-array" count="' + str(frame_total) + '" stride="3">')
   self.writel(S_ANIM, 5, '<param name="X" type="float"/>')
   self.writel(S_ANIM, 5, '<param name="Y" type="float"/>')
   self.writel(S_ANIM, 5, '<param name="Z" type="float"/>')
   self.writel(S_ANIM, 4, '</accessor>')
   self.writel(S_ANIM, 3, '</technique_common>')
   self.writel(S_ANIM, 2, '</source>')

  # Interpolation Source
  self.writel(S_ANIM, 2, '<source id="' +
     anim_id + '-interpolation-output">')
  self.writel(S_ANIM, 3, '<Name_array id="' + anim_id + '-interpolation-output-array" count="' +
     str(frame_total) + '">' + source_interps + '</Name_array>')
  self.writel(S_ANIM, 3, '<technique_common>')
  self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(anim_id) +
     '-interpolation-output-array" count="' + str(frame_total) + '" stride="1">')
  self.writel(S_ANIM, 5, '<param name="INTERPOLATION" type="Name"/>')
  self.writel(S_ANIM, 4, '</accessor>')
  self.writel(S_ANIM, 3, '</technique_common>')
  self.writel(S_ANIM, 2, '</source>')

  self.writel(S_ANIM, 2, '<sampler id="' + anim_id + '-matrix-sampler">')
  self.writel(S_ANIM, 3, '<input semantic="INPUT" source="' +
     self.ref_id(anim_id) + '-input"/>')
  self.writel(S_ANIM, 3, '<input semantic="OUTPUT" source="' +
     self.ref_id(anim_id) + '-matrix-output"/>')
  self.writel(S_ANIM, 3, '<input semantic="INTERPOLATION" source="' +
     self.ref_id(anim_id) + '-interpolation-output"/>')
  self.writel(S_ANIM, 2, '</sampler>')

  if (self.transform_matrix_scale):
   self.writel(S_ANIM, 2, '<sampler id="' +
      anim_id + '-scale-sampler">')
   self.writel(
    S_ANIM, 3, '<input semantic="INPUT" source="' + self.ref_id(anim_id) + '-input"/>')
   self.writel(
    S_ANIM, 3, '<input semantic="OUTPUT" source="' + self.ref_id(anim_id) + '-scale-output"/>')
   self.writel(S_ANIM, 3, '<input semantic="INTERPOLATION" source="' +
      self.ref_id(anim_id) + '-interpolation-output"/>')
   self.writel(S_ANIM, 2, '</sampler>')

  self.writel(S_ANIM, 2, '<channel source="' + self.ref_id(anim_id) +
     '-matrix-sampler" target="' + target + '/transform"/>')
  if (self.transform_matrix_scale):
   self.writel(S_ANIM, 2, '<channel source="' + self.ref_id(anim_id) +
      '-scale-sampler" target="' + target + '/scale"/>')
  if not self.multichannel_single_clip:
   self.writel(S_ANIM, 1, '</animation>')

  return anim_id

 def get_animation_transforms(self, start, end, lookup):


  # get a cache of the scene in rest pose to detect constant and symmetrical
  # animations

  rest_xform_cache = {}
  rest_blend_cache = {}
  self.rest_scene()
  self.cache_scene(rest_xform_cache, rest_blend_cache, lookup, 0)
  self.restore_scene_pose()
  
  frame_orig = self.bpy_context_scene.frame_current

  self.bpy_context_scene.frame_set(start)
  self.settle_ik()

  frame_len = 1.0 / self.bpy_context_scene.render.fps

  xform_cache = {}
  blend_cache = {}
  # Change frames first, export objects last
  # This improves performance enormously

  for t in range(start, end + 1):
   self.bpy_context_scene.frame_set(t)
   key = (t - 1) * frame_len
   self.cache_scene(xform_cache, blend_cache, lookup, key)

  self.bpy_context_scene.frame_set(frame_orig)

  xform_cache = self.xform_cache_without_constants(
   rest_xform_cache, xform_cache)
  blend_cache = self.blend_cache_without_constants(
   rest_blend_cache, blend_cache)

  return xform_cache, blend_cache

 def cache_scene(self, xform_cache, blend_cache, lookup, key):
  linked_nodes=lookup["scene_linked_nodes"]
  for node in self.bpy_context_scene.objects:
   if (not node in self.visual_nodes or node in linked_nodes):
    continue
   if node.data in lookup["mesh_to_morph_controller"]:
    node_id = lookup["nodes"][node]
    morph_controller = lookup["mesh_to_morph_controller"][node.data]
    morph_id = morph_controller["id"]
    targets = morph_controller["targets"]
    for i in range(1, len(node.data.shape_keys.key_blocks)):
     name = node_id
     if node in lookup["node_to_skin"]:
      name += "/source"
     else:
      name += "/morph"
     name += "/" + morph_id + "/" + targets[i - 1][0]
     if (not (name in blend_cache)):
      blend_cache[name] = []

     self.append_morph_keyframe_if_different(
      blend_cache[name], key, node.data.shape_keys.key_blocks[i].value)

   if (node.type == "ARMATURE"):
    # All bones exported for now

    for bone in node.data.bones:

     bone_node_id = lookup["skeleton_info"][node].get(bone, None)
     if not bone_node_id:
      continue

     posebone = node.pose.bones[bone.name]
     transform = self.get_posebone_transform(
      node.pose.bones, posebone)
     if transform:
      if (not (bone_node_id in xform_cache)):
       xform_cache[bone_node_id] = []
      self.append_keyframe_if_different(
       xform_cache[bone_node_id], transform, key)

   transform, visible = self.get_node_local_transform(node)
   if visible:
    node_id = lookup["nodes"][node]
    if (not (node_id in xform_cache)):
     xform_cache[node_id] = []
    self.append_keyframe_if_different(
     xform_cache[node_id], transform, key)

 def is_next_blend_different(self, morph_keyframes, new_value):
  different = False
  if len(morph_keyframes):
   prev_value = morph_keyframes[-1][1]

   if (abs(prev_value - new_value) > (CMP_EPSILON / 100.0)):
    different = True

  return different

 def append_morph_keyframe_if_different(self, morph_keyframes, new_key, new_value):

  if not self.is_next_blend_different(morph_keyframes, new_value):
   if len(morph_keyframes) > 1:
    morph_keyframes.pop()

  morph_keyframes.append((new_key, new_value))

 def blend_cache_without_constants(self, rest_blend_cache, blend_cache):
  # remove morph animations that only have one weight value
  blend_result = {}
  include = False
  for name, keyframes in blend_cache.items():
   if len(keyframes):
    if not self.is_next_blend_different(rest_blend_cache[name], keyframes[0][1]):
     keyframes.remove(keyframes[0])
   if (len(keyframes) > 1):
    include = True
   elif len(keyframes) == 1:
    include = False
   else:
    include = False
   if include:
    blend_result[name] = keyframes
  return blend_result

 def xform_cache_without_constants(self, rest_xform_cache, xform_cache):
  # remove fcurves that have only one transformation over the entire
  # timeline
  xform_result = {}
  for name, transforms in xform_cache.items():
   if len(transforms):
    if not self.is_next_keyframe_different(rest_xform_cache[name], transforms[0][1]):
     transforms.remove(transforms[0])
   if len(transforms) > 1:
    include = True
   elif len(transforms) == 1:
    include = False
   else:
    include = False
   if include:
    xform_result[name] = transforms
  return xform_result

 def is_zero_scale(self, matrix):
  if (matrix[0][0] == 0.0) and (matrix[1][1] == 0) and (matrix[2][2] == 0):
   return True
  else:
   return False

 def append_keyframe_if_different(self, transforms, new_transform, new_key):

  if not self.is_next_keyframe_different(transforms, new_transform):
   if len(transforms) > 1:
    transforms.pop()

  transforms.append((new_key, new_transform))

 def is_next_keyframe_different(self, transforms, new_transform):
  different = False

  if (len(transforms)):
   prev_transform = transforms[-1][1]
   prev_matrix = prev_transform.get("matrix")
   new_matrix = new_transform.get("matrix")
   if (prev_matrix and new_matrix):
    same_matrix = matrix_equal(prev_matrix, new_matrix)
   else:
    same_matrix = True

   different = not same_matrix

   if (not different):
    prev_scale = prev_transform.get("scale")
    new_scale = new_transform.get("scale")
    if (prev_scale and new_scale):
     same_scale = vector_equal(prev_scale, new_scale)
    else:
     same_scale = True
    different = not same_scale
  else:
   different = True

  return different

 def get_NLA_objects(self):
  objects = {}

  for node in self.bpy_context_scene.objects:
   if node.animation_data and node.animation_data.nla_tracks:
    objects[node] = []
    tracks = node.animation_data.nla_tracks
    for track in tracks:
     if track.strips:
      strips = []
      for strip in track.strips:
       strips.append((strip, strip.mute))
      objects[node].append((track, track.mute, strips))

  return objects

 def mute_NLA(self, nla_objects):
  for tracks in nla_objects.values():
   for track in tracks:
    track[0].mute = True
    for strip in track[2]:
     strip[0].mute = True

 def restore_NLA(self, nla_objects):
  for tracks in nla_objects.values():
   for track in tracks:
    track[0].mute = track[1]
    for strip in track[2]:
     strip[0].mute = strip[1]

 def export_animation_clip(self, id, start, end, tcn):
  self.writel(S_ANIM_CLIPS, 1, '<animation_clip id="' + id + '" start="' + strflt((start - 1) /
                      self.bpy_context_scene.render.fps) + '" end="' + strflt((end - 1) / self.bpy_context_scene.render.fps) + '">')
  for z in tcn:
   self.writel(S_ANIM_CLIPS, 2,
      '<instance_animation url="' + self.ref_id(z) + '"/>')
  self.writel(S_ANIM_CLIPS, 1, '</animation_clip>')

 def settle_ik(self):
  # fart around with frames so IK constraints settle into a final
  # position (hopefully)

  frame_orig = self.bpy_context_scene.frame_current
  for i in range(SETTLE_IK_ITERATIONS):
   self.bpy_context_scene.frame_set(frame_orig)

 def mute_timeline(self):
  fcurves = [fcurve for fcurves in
       [animation_data.action.fcurves for animation_data in
     [object.animation_data for object in bpy.data.objects if object.animation_data]
        if animation_data.action] for fcurve in fcurves]
  self.save_fcurve_mute = dict(
   [(fcurve, fcurve.mute) for fcurve in fcurves])
  for fcurve in fcurves:
   fcurve.mute = True

 def unmute_timeline(self):
  for fcurve, mute in self.save_fcurve_mute.items():
   fcurve.mute = mute

 def export_timeline(self, action_name, start, end, lookup):
  xform_cache, blend_cache = self.get_animation_transforms(
   start, end, lookup)
  tcn = []
  if self.multichannel_single_clip:
   animation_id = self.get_node_id(action_name + '-anim')
   self.writel(S_ANIM, 1, '<animation id="' + animation_id + '">')
  for node_id, cache in xform_cache.items():
   tcn.append(self.export_animation_xforms(
    node_id, action_name, cache))
  for target_id, cache in blend_cache.items():
   tcn.append(self.export_animation_blends(
    target_id, action_name, cache))
  if self.multichannel_single_clip:
   self.writel(S_ANIM, 1, '</animation>')
   tcn = [animation_id]
  self.export_animation_clip(action_name, start, end, tcn)

 def unmute_NLA_object(self, node):
  for track in node.animation_data.nla_tracks:
   track.mute = False
   for strip in track.strips:
    strip.mute = False

 def unmute_NLA_track(self, track):
  track.mute = False
  for strip in track.strips:
   strip.mute = False

 def get_NLA_object_timeline(self, node):
  start = sys.float_info.max
  end = -sys.float_info.max
  for track in node.animation_data.nla_tracks:
   for strip in track.strips:
    if start > strip.frame_start:
     start = strip.frame_start
    if end < strip.frame_end:
     end = strip.frame_end
  if start > end:
   start=0
   end=0
  return int(start), int(end)

 def get_NLA_track_timeline(self, track):
  start = sys.float_info.max
  end = -sys.float_info.max
  for strip in track.strips:
   if start > strip.frame_start:
    start = strip.frame_start
   if end < strip.frame_end:
    end = strip.frame_end
  if start > end:
   start=0
   end=0  
  return int(start), int(end)

 def export_animations(self, lookup):

  if self.config["use_anim_timeline"]:
   self.export_timeline(self.get_node_id(self.get_scene_name(self.bpy_context_scene)) + "-timeline", self.bpy_context_scene.frame_start, self.bpy_context_scene.frame_end, lookup)

  if self.config["clip_type"] != 'NONE':
   self.mute_timeline()
   nla = self.get_NLA_objects()

   try:
    if self.config["clip_type"] == 'OBJECT':
     for node in nla.keys():
      self.mute_NLA(nla)
      self.unmute_NLA_object(node)
      start, end = self.get_NLA_object_timeline(node)
      self.export_timeline(self.get_node_id(
       node.name), start, end, lookup)

    if self.config["clip_type"] == 'TRACK':
     for tracks in nla.values():
      for track in tracks:
       self.mute_NLA(nla)
       self.unmute_NLA_track(track[0])
       start, end = self.get_NLA_track_timeline(track[0])
       self.export_timeline(self.get_node_id(
        track[0].name), start, end, lookup)

    if self.config["clip_type"] == 'STRIP':
     self.mute_NLA(nla)
     for tracks in nla.values():
      for track in tracks:
       track[0].mute = False
       for strip in track[2]:
        strip[0].mute = False
        start = int(strip[0].frame_start)
        end = int(strip[0].frame_end)
        self.export_timeline(self.get_node_id(
         strip[0].name), start, end, lookup)
        strip[0].mute = True
       track[0].mute = True

   finally:
    self.restore_NLA(nla)
    self.unmute_timeline()

 def remove_export_meshes(self):
  for me in self.meshes_to_clear:
   me.free_normals_split()
   bpy.data.meshes.remove(me)
  self.meshes_to_clear = []

 def export(self):

  self.writel(S_GEOM, 0, '<library_geometries>')
  self.writel(S_CONT, 0, '<library_controllers>')
  self.writel(S_CAMS, 0, '<library_cameras>')
  self.writel(S_LAMPS, 0, '<library_lights>')
  self.writel(S_IMGS, 0, '<library_images>')
  self.writel(S_MATS, 0, '<library_materials>')
  self.writel(S_FX, 0, '<library_effects>')
  self.writel(S_LIBRARY_NODES, 0, '<library_nodes>')
  self.writel(S_NODES, 0, '<library_visual_scenes>')
  self.writel(S_P_MATS, 0, '<library_physics_materials>')
  self.writel(S_P_MODEL, 0, '<library_physics_models>')
  self.writel(S_P_SCENE, 0, '<library_physics_scenes>')
  self.writel(S_ANIM, 0, '<library_animations>')
  self.writel(S_ANIM_CLIPS, 0, '<library_animation_clips>')

  self.valid_game_types = ['CHARACTER',
         'SENSOR', 'RIGID_BODY', 'DYNAMIC', 'STATIC']
  self.shape_funcs = {
   'BOX': self.export_box_shape,
   'SPHERE': self.export_sphere_shape,
   'CAPSULE': self.export_capsule_shape,
   'CYLINDER': self.export_cylinder_shape,
   'CONE': self.export_cone_shape,
   'CONVEX_HULL': self.export_convex_hull_shape,
   'MESH': self.export_mesh_shape,
   'TRIANGLE_MESH': self.export_mesh_shape}

  lookup = {
    # [material]= id
   "material": {},
   # [material] = effect id
   "effect": {},
    # [image]= id
   "image": {},
   # [mesh] set
   "mesh": {},
   # [mesh]=mesh-convex id
   "convex_mesh": {},
   # [mesh][morph_id, blend value]
   "mesh_morphs": {},
   # [camera(data)]=  id
   "camera": {},
   # [light(data)]=id
   "light": {},
    # [mesh] = [morph_id, targets]
   "mesh_to_morph_controller": {},
   # mesh data to list of [poly group index, target symbol name]
   "node_to_skin": {},
    # [object]= id
   "nodes": {},
    # [armature node][bone]=joint id
   "skeleton_info":{},
   # [node.data/node.game]=id
   "physics_material":{},
   "physics_rigid_body":{},
   "node_to_mesh":{},
   "scene_linked_nodes":{}
  }

  self.bpy_context_scene = bpy.context.scene

  self.export_asset()

  scenes = [scene for scene in bpy.data.scenes]

  for scene in scenes:
   try:
    lookup["scene_linked_nodes"]=set(lookup["nodes"].keys()).intersection(scene.objects)
    self.bpy_context_scene = scene

    self.save_scene_pose()

    self.get_visual_nodes()
    self.rest_scene()

    self.export_materials(lookup)
    self.export_meshes(lookup)
    self.export_cameras(lookup)
    self.export_lights(lookup)
    self.export_morph_controllers(lookup)
    self.export_skin_controllers(lookup)
    self.export_groups(lookup)
    self.export_scene(lookup)

    if self.bpy_context_scene.render.engine == 'BLENDER_GAME':
     physics_nodes = [node
          for node in self.bpy_context_scene.objects
          if (node.game and node.game.physics_type in self.valid_game_types and node in self.visual_nodes) and
          (node.game.use_collision_bounds or node.data in lookup['mesh'])]
     self.export_game_physics_nodes(physics_nodes, lookup)
    else:
     physics_nodes = [node
          for node in self.bpy_context_scene.objects
          if (node.rigid_body and node.rigid_body.collision_shape and node in self.visual_nodes)]
     self.export_physics_nodes(physics_nodes, lookup)

   finally:
    self.restore_scene_pose()
    self.remove_export_meshes()

   self.export_animations(lookup)

  # morphs always go before skin controllers
  if S_MORPH in self.sections:
   for l in self.sections[S_MORPH]:
    self.writel(S_CONT, 0, l)
   del self.sections[S_MORPH]

  # morphs always go before skin controllers
  if S_SKIN in self.sections:
   for l in self.sections[S_SKIN]:
    self.writel(S_CONT, 0, l)
   del self.sections[S_SKIN]

  self.writel(S_CONT, 0, '</library_controllers>')
  self.writel(S_CAMS, 0, '</library_cameras>')
  self.writel(S_LAMPS, 0, '</library_lights>')
  self.writel(S_IMGS, 0, '</library_images>')
  self.writel(S_MATS, 0, '</library_materials>')
  self.writel(S_FX, 0, '</library_effects>')
  self.writel(S_GEOM, 0, '</library_geometries>')
  self.writel(S_LIBRARY_NODES, 0, '</library_nodes>')
  self.writel(S_NODES, 0, '</library_visual_scenes>')
  self.writel(S_P_MODEL, 0, '</library_physics_models>')
  self.writel(S_P_MATS, 0, '</library_physics_materials>')
  self.writel(S_P_SCENE, 0, '</library_physics_scenes>')
  self.writel(S_ANIM_CLIPS, 0, '</library_animation_clips>')
  self.writel(S_ANIM, 0, '</library_animations>')

  try:
   f = open(self.path, "wb")
  except:
   return False
  try:
   f.write(bytes('<?xml version="1.0" encoding="utf-8"?>\n', "UTF-8"))
   f.write(bytes(
    '<COLLADA xmlns="http://www.collada.org/2008/03/COLLADASchema" version="1.5.0">\n', "UTF-8"))

   s = []
   for x in self.sections.keys():
    s.append(x)
   s.sort()
   for x in s:
    for l in self.sections[x]:
     f.write(bytes(l + "\n", "UTF-8"))

   f.write(bytes('<scene>\n', "UTF-8"))
   scene = bpy.context.scene
   f.write(bytes('\t<instance_visual_scene url="' +
        self.ref_id(self.get_scene_name(scene)) + '"/>\n', "UTF-8"))
   f.write(bytes('\t<instance_physics_scene url="' +
        self.ref_id(self.get_scene_name(scene)) + '-physics' + '"/>\n', "UTF-8"))
   f.write(bytes('</scene>\n', "UTF-8"))
   f.write(bytes('</COLLADA>\n', "UTF-8"))
  finally:
   f.close()

  return True

 def get_scene_name(self, scene):
  return scene.name

 def __init__(self, path, kwargs, operator):
  self.operator = operator
  self.last_id = 0
  self.sections = {}
  self.path = path
  self.config = kwargs
  self.visual_nodes = set()
  self.meshes_to_clear = []
  self.node_names = {}
  self.transform_matrix_scale = self.config["transform_type"] == 'MATRIX_SCALE'
  self.image_cache = set()
  self.triangulate = False
  self.multichannel_single_clip = False
  self.axis_type = self.config['axis_type']
  self.use_tangents = False
  self.always_tangent = False
  if self.config['tangents'] == 'NONE':
   self.use_tangents = False
  elif self.config['tangents'] == 'BUMPED':
   self.use_tangents = True
   self.always_tangent = False
  elif self.config['tangents'] == 'ALWAYS':
   self.use_tangents = True
   self.always_tangent = True
  self.overstuff_bones = False
  self.use_active_layers = False
  self.use_mesh_modifiers=True

def save(operator, context,
   filepath="",
   use_selection=False,
   **kwargs
   ):

 exp = DaeExporter(filepath, kwargs, operator)
 exp.export()
 return {'FINISHED'}  # so the script wont run after we have batch exported.
