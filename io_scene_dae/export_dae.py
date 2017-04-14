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
S_NODES = 10
S_P_MATS = 11
S_P_MODEL = 12
S_P_SCENE = 13
S_ANIM = 14
S_ANIM_CLIPS = 15

CMP_EPSILON = 0.01

def strmtx(mtx):
	return " ".join([str(e) for v in mtx for e in v])

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
	" ".join([str(x * mult) for x in a])
	s = " "
	for x in a:
		s += " " + str(x * mult)
	if len(a) == 3:
		s += " 1.0"
	s += " "
	return s

def strarr(arr):
	return " ".join([str(e) for e in arr])

class DaeExporter:

	def new_id(self, t):
		self.last_id += 1
		return "id-" + t + "-" + str(self.last_id)

	def clean_bone_name(self, name):
		if (" ".find(name) != -1):
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

	def export_image(self, image, image_id):
		imgpath = image.filepath
		if (imgpath.find("//") == 0 or imgpath.find("\\\\") == 0):
			# if relative, convert to absolute
			imgpath = bpy.path.abspath(imgpath)

		# path is absolute, now do something!

		if (self.config["use_copy_images"]):
			# copy image
			basedir = os.path.dirname(self.path) + "/images"
			if (not os.path.isdir(basedir)):
				os.makedirs(basedir)
			if os.path.isfile(imgpath):
				dstfile = basedir + "/" + os.path.basename(imgpath)
				# if (not os.path.isfile(dstfile)):
				shutil.copy(imgpath, dstfile)
				imgpath = "images/" + os.path.basename(imgpath)
			else:
				# ## if file is not found save it as png file in the destination folder
				img_tmp_path = image.filepath
				if img_tmp_path.endswith((".bmp", ".rgb", ".png", ".jpeg", ".jpg", ".jp2", ".tga", ".cin", ".dpx", ".exr", ".hdr", ".tif")):
					image.filepath = basedir + "/" + os.path.basename(img_tmp_path)
				else:
					image.filepath = basedir + "/" + image.name + ".png"

				dstfile = basedir + "/" + os.path.basename(image.filepath)

				# if (not os.path.isfile(dstfile)):
				try:
					image.save()
				except:
					pass

				imgpath = "images/" + os.path.basename(image.filepath)
				image.filepath = img_tmp_path
		else:
			# export relative, always, no one wants absolute paths.
			try:
				imgpath = os.path.relpath(imgpath, os.path.dirname(self.path)).replace("\\", "/")  # export unix compatible always

			except:
				pass  # fails sometimes, not sure why

		self.writel(S_IMGS, 1, '<image id="' + image_id + '" name="' + image.name + '">')
		
		# the file path should be surrounded by <ref> tags
		
		self.writel(S_IMGS, 2, '<init_from><ref>' + imgpath + '</ref></init_from>')
		self.writel(S_IMGS, 1, '</image>')

	def export_effect(self, material, effect_id, image_lookup):

		self.writel(S_FX, 1, '<effect id="' + effect_id + '" name="' + material.name + '">')
		self.writel(S_FX, 2, '<profile_COMMON>')

		# Find and fetch the textures and create sources
		sampler_table = {}
		diffuse_tex = None
		specular_tex = None
		emission_tex = None
		normal_tex = None
		ambient_tex = None
		done_images = set()
		
		for i in range(len(material.texture_slots)):
			ts = material.texture_slots[i]
			if (not ts):
				continue
			if (not ts.texture):
				continue
			if (not hasattr(ts.texture, "image")):
				continue
			if (ts.texture.image == None):
				continue

			# image
			imgid = image_lookup.get(ts.texture.image, None)
			if (not imgid):
				continue
			if (imgid in done_images):
				continue
			done_images.add(imgid)
			
			sampler_sid = imgid + "-sampler"
			self.writel(S_FX, 3, '<newparam sid="' + sampler_sid + '">')
			self.writel(S_FX, 4, '<sampler2D>')
			self.writel(S_FX, 5, '<instance_image url="{}"/>'.format(imgid))
			self.writel(S_FX, 4, '</sampler2D>')
			self.writel(S_FX, 3, '</newparam>')
			sampler_table[i] = sampler_sid

			if (ts.use_map_color_diffuse and diffuse_tex == None):
				diffuse_tex = sampler_sid
			if (ts.use_map_color_spec and specular_tex == None):
				specular_tex = sampler_sid
			if (ts.use_map_emit and emission_tex == None):
				emission_tex = sampler_sid
			if (ts.use_map_normal and normal_tex == None):
				normal_tex = sampler_sid
			if (ts.use_map_ambient and ambient_tex == None):
				ambient_tex = sampler_sid
			

		self.writel(S_FX, 3, '<technique sid="common">')
		shtype = "blinn"
		self.writel(S_FX, 4, '<' + shtype + '>')
		# ambient? from where?

		self.writel(S_FX, 5, '<emission>')
		if (emission_tex != None):
			self.writel(S_FX, 6, '<texture texture="' + emission_tex + '/>')
		else:
			self.writel(S_FX, 6, '<color>' + numarr_alpha(material.diffuse_color, material.emit) + ' </color>')  # not totally right but good enough
		self.writel(S_FX, 5, '</emission>')
		self.writel(S_FX, 5, '<ambient>')
		if (ambient_tex != None):
			self.writel(S_FX, 6, '<texture texture="' + ambient_tex + '" texcoord="ambient"/>')
		else:
			self.writel(S_FX, 6, '<color>' + numarr_alpha(self.scene.world.ambient_color, material.ambient) + ' </color>')
		self.writel(S_FX, 5, '</ambient>')
		self.writel(S_FX, 5, '<diffuse>')
		if (diffuse_tex != None):
			self.writel(S_FX, 6, '<texture texture="' + diffuse_tex + '" texcoord="diffuse"/>')
		else:
			self.writel(S_FX, 6, '<color>' + numarr_alpha(material.diffuse_color, material.diffuse_intensity) + '</color>')
		self.writel(S_FX, 5, '</diffuse>')
		self.writel(S_FX, 5, '<specular>')
		if (specular_tex != None):
			self.writel(S_FX, 6, '<texture texture="' + specular_tex + '" texcoord="specular"/>')
		else:
			self.writel(S_FX, 6, '<color>' + numarr_alpha(material.specular_color, material.specular_intensity) + '</color>')
		self.writel(S_FX, 5, '</specular>')
		self.writel(S_FX, 5, '<shininess>')
		self.writel(S_FX, 6, '<float>' + str(material.specular_hardness) + '</float>')
		self.writel(S_FX, 5, '</shininess>')
		self.writel(S_FX, 5, '<reflective>')
		self.writel(S_FX, 6, '<color>' + numarr_alpha(material.mirror_color) + '</color>')
		self.writel(S_FX, 5, '</reflective>')

		if (material.use_transparency):
			self.writel(S_FX, 5, '<transparency>')
			self.writel(S_FX, 6, '<float>' + str(material.alpha) + '</float>')
			self.writel(S_FX, 5, '</transparency>')

		self.writel(S_FX, 5, '<index_of_refraction>')
		self.writel(S_FX, 6, '<float>' + str(material.specular_ior) + '</float>')
		self.writel(S_FX, 5, '</index_of_refraction>')
		self.writel(S_FX, 4, '</' + shtype + '>')
		if (normal_tex):
			self.writel(S_FX, 4, '<bump bumptype="NORMALMAP">')
			self.writel(S_FX, 5, '<texture texture="' + normal_tex + '" texcoord="normal"/>')
			self.writel(S_FX, 4, '</bump>')
		self.writel(S_FX, 3, '</technique>')
		self.writel(S_FX, 2, '</profile_COMMON>')
		self.writel(S_FX, 1, '</effect>')

	def node_to_mesh(self, node, triangulate):
		apply_modifiers = len(node.modifiers) and self.config["use_mesh_modifiers"]
	
			# get a mesh for this node
		try:
			mesh = node.to_mesh(self.scene, apply_modifiers, "RENDER")  # is this allright?
		except:
			return None
				
		if not mesh or not len(mesh.polygons):
			# mesh has no polygons so abort
			if mesh and (mesh != node.data): 
				bpy.data.meshes.remove(mesh)
			return False

		# mesh.calc_normals_split();
		
		# force triangulation if the mesh has polygons with more than 4 sides
		force_triangluation = False
		if not triangulate:
			for polygon in mesh.polygons:
				if (polygon.loop_total > 4):
					force_triangluation = True
					break
		
		if (triangulate or force_triangluation):
			bm = bmesh.new()
			bm.from_mesh(mesh)
			bmesh.ops.triangulate(bm, faces=bm.faces)
			bm.to_mesh(mesh)
			bm.free()
			# mesh.calc_normals_split();
			mesh.update(calc_tessface=True)
		
		if (mesh != node.data):
			self.meshes_to_clear.append(mesh)		
		return mesh
		
	def calculate_tangents(self, mesh):
		use_tangents = False
		if (use_tangents and len(mesh.uv_textures)):
			try:
				mesh.calc_tangents()
				return True 
			except:
				mesh.calc_normals_split()
				return False
		else:
			mesh.calc_normals_split()
			return False

	def average_color(self, color, count):
		if count != 0:
			return Color(color / count)
		else:
			return Color(color)
		
	def get_mesh_surfaces(self, mesh):
		# Turn the mesh into buffers and index buffers, polygons are grouped by material index
		
		# get vertices
		vertices = [Vector(v.co) for v in mesh.vertices.values()]

		# get polygons
		loop_vertices = self.get_polygon_groups(mesh)

		# convert dictionary of loop vertices to dictionary of vertex indices
		# v[0] is loop index, v[1] is the vertex 
		surface_v_indices = {g:s for (g, s) in 
							zip(loop_vertices.keys(),
										[[[v.vertex_index for v in p] for p in g] for g in loop_vertices.values()])}
		
		# convert dictionary of loop vertices to a flat list of normals, removing duplicates
		normals = list(set([v.normal.freeze() for v in mesh.vertices.values()]))
		
		if (not ((len(normals) == 1) and (normals[0].length < 0.1))):
			normals_map = {k:v for (v, k) in enumerate(normals)}
			# convert dictionary of loop vertices to a dictionary of normal indices
			surface_normal_indices = {g:s for (g, s) in 
									zip(loop_vertices.keys(), [[[normals_map[mesh.vertices[v.vertex_index].normal.freeze()] for v in p] for p in g] for g in loop_vertices.values()])}
		else:
			# if no normals on the mesh then the normals list will be just one zero vector
			normals = []
			surface_normal_indices = {}
		
		# get uv's
		if (mesh.uv_layers != None) and (mesh.uv_layers.active != None):
			uv_layer = mesh.uv_layers.active.data
		else:
			uv_layer = None
		
		uv = []
		if (uv_layer != None):
			# get all uv values, removing duplicates
			uv = list(set([uv.uv.freeze() for uv in uv_layer.values()]))

		surface_uv_indices = {}
		if (len(uv)):
			uv_map = {k:v for (v, k) in enumerate(uv)}
			# convert dictionary of loop vertices into a dictionary of uv indices (into the uv list)
			surface_uv_indices = {g:s for (g, s) in 
							zip(loop_vertices.keys(),
										[[[uv_map[uv_layer[v.index].uv.freeze()] for v in p] for p in g] for g in loop_vertices.values()])}


		if (mesh.vertex_colors):
			# Blender doesn't have colors per vertex instead color per polygon loop vertex. 
			# So guess vertex colors by averaging every color used for each vertex.
			# colors are aligned with vertices
			color_buckets = [[0, Vector((0, 0, 0))] for i in range(len(vertices))]
			for loop_vertex in [v for g in loop_vertices.values() for p in g for v in p]:
				color_buckets[loop_vertex.vertex_index][0] += 1
				color_buckets[loop_vertex.vertex_index][1] += Vector(mesh.vertex_colors.active.data[loop_vertex.index].color)
				
			colors = [self.average_color(color, count) for (count, color) in color_buckets if count != 0]
		else:
			colors = []
			
		# vertices = array of xyz point tuples
		# normals = array of xyz vector tuples 
		# uv = array of uv point tuples
		# colors = array of r,g,b color tuples associated with vertices array
		# surface_v_indices = Key: is the material slot index for the group of polygons, Values: is a list of integer vertex indices (into vertices array)
		# surface_normal_indices =  Key: is the material slot index for the group of polygons,Values: is a list of integer normal indices (into normals array)
		# surface_uv_indices = Key: is the material slot index for the group of polygons,Values: is a list of integer uv indices (into uv array)
		return vertices, normals, uv, colors, surface_v_indices, surface_normal_indices, surface_uv_indices
	
	def get_polygon_groups(self, mesh):
		# get a dictionary of polygons with loop vertices grouped by material
		vertices = {}
		
		# get the material for each corresponding dictionary entry in vertices
		materials = {}
		
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
		
		return	vertices

	def node_has_convex_hull(self, node):
		if (not self.config["use_physics"]):
			return False
		if (
			node.rigid_body and 
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
			ret = bmesh.ops.convex_hull(bm, input=bm.verts, use_existing_faces=False)
			
			# Delete the vertices that weren't used in the convex hull
			
			geom = ret["geom_unused"]
			# there is no documentation for values of 'context'. 1 works to delete vertices
			bmesh.ops.delete(bm, geom=geom, context=1)
		except:
			return None

		# convert bmesh back to mesh	
		me = bpy.data.meshes.new(self.new_id("DAE_convex_hull"))
		bm.to_mesh(me)
		if (me != mesh):
			self.meshes_to_clear.append(me)
		return me
			
	def export_meshes(self):
		meshes = 	set(
			filter(lambda obj :	obj.type == "MESH" or obj.type == "CURVE", 	self.valid_nodes)
		);
			
		triangulate = self.config["use_triangles"]
		geometry_lookup = {}
		convex_geometry_lookup = {}
		geometry_morphs = {}
		material_bind_lookup = {}
		for node in meshes:
			if (node.data in geometry_lookup):
				# already exported
				continue
			
			# generate mesh from node
			
			mesh = self.node_to_mesh(node, triangulate)
			
			# export the mesh
			if (mesh):
				mesh_id = self.get_node_id(node.data.name + "-mesh")
				material_bind = self.export_mesh(mesh, mesh_id, node.data.name, triangulate)
				geometry_lookup[node.data] = mesh_id
				if (len(material_bind)):
					material_bind_lookup[node.data] = material_bind
					
				# export convex hull if needed by physics scene
				
				if (self.node_has_convex_hull(node)):
					convex_mesh = self.mesh_to_convex_hull(mesh)
					if (convex_mesh):
						convex_mesh_id = mesh_id + "-convex"
						valid, material_bind = self.export_mesh(convex_mesh, convex_mesh_id, node.data.name, triangulate, True)
						if (valid):
							convex_geometry_lookup[node.data] = convex_mesh_id
							
				# export morphs from shape keys
				
				morphs = self.export_mesh_morphs(node, mesh_id)
				if (morphs):
					geometry_morphs[node.data] = morphs
					
			else:
				if(node.type == "CURVE"):
					# All else failed so export a Bezier curve
					curve_id = self.get_node_id(node.data.name + "-curve")
					self.export_curve(node.data, curve_id)
					geometry_lookup[node.data] = curve_id
					
		self.remove_export_meshes()
			
		return geometry_lookup, convex_geometry_lookup, geometry_morphs, material_bind_lookup
	
	def export_mesh_morphs(self, node, mesh_id):
		mesh = node.data
		triangulate = self.config["use_triangles"]

		if (mesh.shape_keys != None and len(mesh.shape_keys.key_blocks)):
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
				morph_id = self.get_node_id(mesh_id + "-morph-" + shape.name)
				morph_targets.append([morph_id, values[k - 1]])
				export_mesh = self.node_to_mesh(node, triangulate)
				self.export_mesh(export_mesh, morph_id, morph_id, triangulate)
				shape.value = values[k - 1]
			
			node.show_only_shape_key = scene_show_only_shape_key
			node.active_shape_key_index = scene_active_shape_key
			mesh.update
			
			# morph_targets = [id of morph shape mesh, the shape key index corrected to remove the basis (shape key 1 is index 0)]
			return morph_targets
		else:	
			return None;
	
	def export_morph_controllers(self, mesh_lookup, geometry_morphs):
		morph_lookup = {}
		for mesh, targets in geometry_morphs.items():
			morph_id = self.get_node_id(mesh.name + "-morph")
			morph_lookup[mesh] = [morph_id, targets]
			mesh_id = mesh_lookup[mesh]
			self.export_morph_controller(mesh_id, targets, morph_id)
		return morph_lookup
		
	def export_morph_controller(self, mesh_id, morph_targets, morph_id):
			self.writel(S_MORPH, 1, '<controller id="' + morph_id + '" name="' + morph_id + '">')
			self.writel(S_MORPH, 2, '<morph source="#' + mesh_id + '" method="NORMALIZED">')
			self.writel(S_MORPH, 3, '<source id="' + morph_id + '-targets">')
			self.writel(S_MORPH, 4, '<IDREF_array id="' + morph_id + '-targets-array" count="' + str(len(morph_targets)) + '">')
			marr = " ".join([name[0] for name in morph_targets])  
			warr = " ".join([str(weight[1]) for weight in morph_targets])
			self.writel(S_MORPH, 5, marr)
			self.writel(S_MORPH, 4, '</IDREF_array>')
			self.writel(S_MORPH, 4, '<technique_common>')
			self.writel(S_MORPH, 5, '<accessor source="#' + morph_id + '-targets-array" count="' + str(len(morph_targets)) + '" stride="1">')
			self.writel(S_MORPH, 6, '<param name="MORPH_TARGET" type="IDREF"/>')
			self.writel(S_MORPH, 5, '</accessor>')
			self.writel(S_MORPH, 4, '</technique_common>')
			self.writel(S_MORPH, 3, '</source>')
			self.writel(S_MORPH, 3, '<source id="' + morph_id + '-weights">')
			self.writel(S_MORPH, 4, '<float_array id="' + morph_id + '-weights-array" count="' + str(len(morph_targets)) + '" >')
			self.writel(S_MORPH, 5, warr)
			self.writel(S_MORPH, 4, '</float_array>')
			self.writel(S_MORPH, 4, '<technique_common>')
			self.writel(S_MORPH, 5, '<accessor source="#' + morph_id + '-weights-array" count="' + str(len(morph_targets)) + '" stride="1">')
			self.writel(S_MORPH, 6, '<param name="MORPH_WEIGHT" type="float"/>')
			self.writel(S_MORPH, 5, '</accessor>')
			self.writel(S_MORPH, 4, '</technique_common>')
			self.writel(S_MORPH, 3, '</source>')
			self.writel(S_MORPH, 3, '<targets>')
			self.writel(S_MORPH, 4, '<input semantic="MORPH_TARGET" source="#' + morph_id + '-targets"/>')
			self.writel(S_MORPH, 4, '<input semantic="MORPH_WEIGHT" source="#' + morph_id + '-weights"/>')
			self.writel(S_MORPH, 3, '</targets>')
			self.writel(S_MORPH, 2, '</morph>')
			self.writel(S_MORPH, 1, '</controller>')

	def export_skin_controllers(self, mesh_lookup, morph_lookup):
		meshes = 	set(
			filter(lambda obj : (obj.type == "MESH") and (obj.data in mesh_lookup.keys()), self.valid_nodes)
		);
		
		skin_controller_lookup = {}
		
		for node in meshes:
			if not node.data in skin_controller_lookup.keys():
				armatures = [mod for mod in node.modifiers.values() if (mod.type == "ARMATURE") and mod.use_vertex_groups]
				for armature in armatures:
					skin_id = self.get_node_id(node.data.name + "-" + armature.object.name + "-skin")
					lu = {"skin":skin_id, "skeleton":armature.object.name}
					if (node.data in skin_controller_lookup):
						skin_controller_lookup[node.data].append(lu)
					else:
						skin_controller_lookup[node.data] = [lu]
					if (node.data in morph_lookup):
						mesh_id = morph_lookup[node.data][0]
					else:
						mesh_id = mesh_lookup[node.data]
					self.export_skin_controller(node, armature.object, mesh_id , skin_id)
		return skin_controller_lookup	
		
	def export_skin_controller(self, node, armature, mesh_id, skin_id):
		group_names = [group.name for group in node.vertex_groups.values()]
		bones = [armature.data.bones[name] for name in group_names if name in armature.data.bones.keys()]
		pose_matrices = [(armature.matrix_world * bone.matrix_local).inverted() for bone in bones]
		weight_counts = [len(v.groups) for v in node.data.vertices]
		weights = list(set([group.weight for v in node.data.vertices for group in v.groups]))
		weights_index = {k:v for (v, k) in enumerate(weights)}
		bone_weights = [i for w in
					 [
					 	[g.group, weights_index[g.weight]] 
					 		for v in node.data.vertices for g in v.groups
					 ] 
					for i in w]
		
		self.writel(S_SKIN, 1, '<controller id="' + skin_id + '">')
		self.writel(S_SKIN, 2, '<skin source="#' + mesh_id + '">')
		self.writel(S_SKIN, 3, '<bind_shape_matrix>' + strmtx(node.matrix_world) + '</bind_shape_matrix>')
		
		# Joint Names
		
		self.writel(S_SKIN, 3, '<source id="' + skin_id + '-joints">')
		name_values = " ".join([self.clean_bone_name(name) for name in group_names])
		self.writel(S_SKIN, 4, '<Name_array id="' + skin_id + '-joints-array" count="' + str(len(group_names)) + '">' + name_values + '</Name_array>')
		self.writel(S_SKIN, 4, '<technique_common>')
		self.writel(S_SKIN, 4, '<accessor source="#' + skin_id + '-joints-array" count="' + str(len(group_names)) + '" stride="1">')
		self.writel(S_SKIN, 5, '<param name="JOINT" type="Name"/>')
		self.writel(S_SKIN, 4, '</accessor>')
		self.writel(S_SKIN, 4, '</technique_common>')
		self.writel(S_SKIN, 3, '</source>')

		# Pose Matrices!
		
		self.writel(S_SKIN, 3, '<source id="' + skin_id + '-bind_poses">')
		pose_values = " ".join([strmtx(matrix) for matrix in pose_matrices])
		self.writel(S_SKIN, 4, '<float_array id="' + skin_id + '-bind_poses-array" count="' + str(len(pose_matrices) * 16) + '">' + pose_values + '</float_array>')
		self.writel(S_SKIN, 4, '<technique_common>')
		self.writel(S_SKIN, 4, '<accessor source="#' + skin_id + '-bind_poses-array" count="' + str(len(pose_matrices)) + '" stride="16">')
		self.writel(S_SKIN, 5, '<param name="TRANSFORM" type="float4x4"/>')
		self.writel(S_SKIN, 4, '</accessor>')
		self.writel(S_SKIN, 4, '</technique_common>')
		self.writel(S_SKIN, 3, '</source>')

		# Skin Weights!
		
		self.writel(S_SKIN, 3, '<source id="' + skin_id + '-weights">')
		self.writel(S_SKIN, 4, '<float_array id="' + skin_id + '-weights-array" count="' + str(len(weights)) + '">' + " ".join([str(w) for w in weights]) + '</float_array>')
		self.writel(S_SKIN, 4, '<technique_common>')
		self.writel(S_SKIN, 4, '<accessor source="#' + skin_id + '-weights-array" count="' + str(len(weights)) + '" stride="1">')
		self.writel(S_SKIN, 5, '<param name="WEIGHT" type="float"/>')
		self.writel(S_SKIN, 4, '</accessor>')
		self.writel(S_SKIN, 4, '</technique_common>')
		self.writel(S_SKIN, 3, '</source>')
		self.writel(S_SKIN, 3, '<joints>')
		self.writel(S_SKIN, 4, '<input semantic="JOINT" source="#' + skin_id + '-joints"/>')
		self.writel(S_SKIN, 4, '<input semantic="INV_BIND_MATRIX" source="#' + skin_id + '-bind_poses"/>')
		self.writel(S_SKIN, 3, '</joints>')
		self.writel(S_SKIN, 3, '<vertex_weights count="' + str(len(weight_counts)) + '">')
		self.writel(S_SKIN, 4, '<input semantic="JOINT" source="#' + skin_id + '-joints" offset="0"/>')
		self.writel(S_SKIN, 4, '<input semantic="WEIGHT" source="#' + skin_id + '-weights" offset="1"/>')
		self.writel(S_SKIN, 4, '<vcount>' + " ".join([str(c) for c in weight_counts]) + '</vcount>')
		self.writel(S_SKIN, 4, '<v>' + " ".join([str(i) for i in bone_weights]) + '</v>')
		self.writel(S_SKIN, 3, '</vertex_weights>')
		self.writel(S_SKIN, 2, '</skin>')
		self.writel(S_SKIN, 1, '</controller>')

	def export_mesh(self, mesh, mesh_id, mesh_name, triangulated, convex=False):

		vertices, normals, uv, colors, surface_v_indices, surface_normal_indices, surface_uv_indices = self.get_mesh_surfaces(mesh)
		
		has_vertex = len(vertices) > 0	
		has_normals = len(normals) > 0
		has_uv = len(uv) > 0	
		has_colors = has_vertex and len(colors) > 0
		
		self.writel(S_GEOM, 1, '<geometry id="' + mesh_id + '" name="' + mesh_name + '">')
		if (convex):
			self.writel(S_GEOM, 2, '<convex_mesh>')
		else:
			self.writel(S_GEOM, 2, '<mesh>')

		# Vertex Array
		if (has_vertex):
			self.writel(S_GEOM, 3, '<source id="' + mesh_id + '-positions">')
			float_values = " ".join([str(c) for v in [[v.x, v.y, v.z] for v in vertices] for c in v])
			self.writel(S_GEOM, 4, '<float_array id="' + mesh_id + '-positions-array" count="' + str(len(vertices) * 3) + '">' + float_values + '</float_array>')
			self.writel(S_GEOM, 4, '<technique_common>')
			self.writel(S_GEOM, 5, '<accessor source="#' + mesh_id + '-positions-array" count="' + str(len(vertices)) + '" stride="3">')
			self.writel(S_GEOM, 6, '<param name="X" type="float"/>')
			self.writel(S_GEOM, 6, '<param name="Y" type="float"/>')
			self.writel(S_GEOM, 6, '<param name="Z" type="float"/>')
			self.writel(S_GEOM, 5, '</accessor>')
			self.writel(S_GEOM, 4, '</technique_common>')
			self.writel(S_GEOM, 3, '</source>')

		# Normal Array
		if (has_normals):
			self.writel(S_GEOM, 3, '<source id="' + mesh_id + '-normals">')
			float_values = " ".join([str(c) for v in [[v.x, v.y, v.z] for v in normals] for c in v])
			self.writel(S_GEOM, 4, '<float_array id="' + mesh_id + '-normals-array" count="' + str(len(normals) * 3) + '">' + float_values + '</float_array>')
			self.writel(S_GEOM, 4, '<technique_common>')
			self.writel(S_GEOM, 5, '<accessor source="#' + mesh_id + '-normals-array" count="' + str(len(normals)) + '" stride="3">')
			self.writel(S_GEOM, 6, '<param name="X" type="float"/>')
			self.writel(S_GEOM, 6, '<param name="Y" type="float"/>')
			self.writel(S_GEOM, 6, '<param name="Z" type="float"/>')
			self.writel(S_GEOM, 5, '</accessor>')
			self.writel(S_GEOM, 4, '</technique_common>')
			self.writel(S_GEOM, 3, '</source>')

		# UV Arrays
		if (has_uv):
			self.writel(S_GEOM, 3, '<source id="' + mesh_id + '-texcoord">')
			float_values = " ".join([str(c) for v in [[v.x, v.y] for v in uv] for c in v])
			self.writel(S_GEOM, 4, '<float_array id="' + mesh_id + '-texcoord-array" count="' + str(len(uv) * 2) + '">' + float_values + '</float_array>')
			self.writel(S_GEOM, 4, '<technique_common>')
			self.writel(S_GEOM, 5, '<accessor source="#' + mesh_id + '-texcoord-array" count="' + str(len(uv)) + '" stride="2">')
			self.writel(S_GEOM, 6, '<param name="S" type="float"/>')
			self.writel(S_GEOM, 6, '<param name="T" type="float"/>')
			self.writel(S_GEOM, 5, '</accessor>')
			self.writel(S_GEOM, 4, '</technique_common>')
			self.writel(S_GEOM, 3, '</source>')

		# Color Arrays

		if (has_colors):
			self.writel(S_GEOM, 3, '<source id="' + mesh_id + '-colors">')
			float_values = " ".join([str(c) for v in [[v.r, v.g, v.b] for v in colors] for c in v])
			self.writel(S_GEOM, 4, '<float_array id="' + mesh_id + '-colors-array" count="' + str(len(colors) * 3) + '">' + float_values + '</float_array>')
			self.writel(S_GEOM, 4, '<technique_common>')
			self.writel(S_GEOM, 5, '<accessor source="#' + mesh_id + '-colors-array" count="' + str(len(colors)) + '" stride="3">')
			self.writel(S_GEOM, 6, '<param name="R" type="float"/>')
			self.writel(S_GEOM, 6, '<param name="G" type="float"/>')
			self.writel(S_GEOM, 6, '<param name="B" type="float"/>')
			self.writel(S_GEOM, 5, '</accessor>')
			self.writel(S_GEOM, 4, '</technique_common>')
			self.writel(S_GEOM, 3, '</source>')

		# Triangle Lists
		self.writel(S_GEOM, 3, '<vertices id="' + mesh_id + '-vertices">')
		self.writel(S_GEOM, 4, '<input semantic="POSITION" source="#' + mesh_id + '-positions"/>')
		self.writel(S_GEOM, 3, '</vertices>')

		prim_type = ""
		if (triangulated):
			prim_type = "triangles"
		else:
			prim_type = "polylist"
			
		# calculate offsets and layout of <p> indices			
		offset = 0
		if (has_vertex):
			vertex_offset = offset
			offset += 1
		if (has_normals):
			normal_offset = offset
			offset += 1
		if (has_uv):
			uv_offset = offset
			offset += 1
		if (has_colors):
			color_offset = vertex_offset
		stride = offset
		
		material_bind = {}
		
		for mat_index, polygons in surface_v_indices.items():
			
			# Every renderable mesh must have a material symbol even if no material is assigned in Blender.
			matref = self.get_material_link_symbol("target")
			material_bind[mat_index] = matref

			self.writel(S_GEOM, 3, '<' + prim_type + ' count="' + str(len(polygons)) + '" material="' + matref + '">')  # todo material
			if (has_vertex):
				self.writel(S_GEOM, 4, '<input semantic="VERTEX" source="#' + mesh_id + '-vertices" offset="' + str(vertex_offset) + '"/>')
			if (has_normals):
				self.writel(S_GEOM, 4, '<input semantic="NORMAL" source="#' + mesh_id + '-normals" offset="' + str(normal_offset) + '"/>')
			if (has_uv):
				self.writel(S_GEOM, 4, '<input semantic="TEXCOORD" source="#' + mesh_id + '-texcoord" offset="' + str(uv_offset) + '" set="0"/>')
			if (has_colors):
				self.writel(S_GEOM, 4, '<input semantic="COLOR" source="#' + mesh_id + '-colors" offset="' + str(color_offset) + '"/>')
			
			# vcount list if not triangulating, as a triangle always has 3 sides no need for an array of 3s
			if (not triangulated):
				int_values = "<vcount>"
				int_values += " ".join([str(len(p)) for p in polygons])
				int_values += "</vcount>"
				self.writel(S_GEOM, 4, int_values)
				
			# faces
			int_values = "<p>"

			index_buffer = []
			indices = [0 for i in range(stride)]
			for p in range(0, len(polygons)):
				group_polygons = polygons[p]
				normal_indices = None
				uv_indices = None
				if has_normals:
					normal_indices = surface_normal_indices[mat_index][p]
				if has_uv:
					uv_indices = surface_uv_indices[mat_index][p]
				for i in range(0, len(polygons[p])):
					if (has_vertex):
						indices[vertex_offset] = group_polygons[i]
					if (has_normals):
						indices[normal_offset] = normal_indices[i]
					if (has_uv):
						indices[uv_offset] = uv_indices[i]
					# if (has_colors):
					# 	indices[color_offset] = group_polygons[i]
					index_buffer.append(indices.copy())
			int_values += " ".join([str(i) for c in index_buffer for i in c])
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

	def get_material_link_symbol(self, material):
		# The material symbol is a proxy name for when a material is linked to a mesh.
		# This generates a a unique id for each call, even if the same material passed more than once.
		return self.get_node_id(material + "-material-symbol")
	
	def get_node_id(self, name):
		count = self.node_names.get(name, 0)
		node_id = name
		if (count > 0):
			node_id += "-" + str(count)
		self.node_names[name] = count + 1
		return node_id

	def export_armature_bone(self, bone, il, si, lookup, nodes_lookup, parenting_map):
		deforming = bone.use_deform
		
		# don't export bones that don't deform if the option is set. Children nodes are still exported
		do_export = deforming or not self.config['use_deform_bone_only']
		
		if (do_export):
			boneid = self.get_node_id(bone.name)
			bonesid = bone.name
			si["bone_ids"][bone] = boneid
			self.writel(S_NODES, il, '<node id="' + boneid + '" sid="' + bonesid + '" name="' + bone.name + '" type="JOINT">')
			il += 1
	
			# don't put a transform if the bone doesn't deform
			
			if (deforming):
				transforms = self.get_bone_transform_xml(bone)
				for t in transforms:
					self.writel(S_NODES, il, t)
				si["has_transform"][bone] = True
			else:
				si["has_transform"][bone] = False
	
		# export nodes that are parented to this bone
		
		node_children = parenting_map.get(bone.name, None)
		if (node_children != None):
			for c in node_children:
				self.export_node(c, il, lookup, nodes_lookup)
			
		for c in bone.children:
			self.export_armature_bone(c, il, si, lookup, nodes_lookup, parenting_map)
		il -= 1
		if (do_export):
			self.writel(S_NODES, il, '</node>')

	def export_armature_node(self, node, il, lookup, nodes_lookup):
		
		# Export the rig in rest mode, this ensures that geometry attached to bones are also at the rest position
		
		pose_position = node.data.pose_position
		if (pose_position == 'POSE'):
			node.data.pose_position = 'REST'
			bpy.context.scene.update();
		
		try:	
			parenting_map = self.get_armature_children_of_bones(node)
			self.skeletons.append(node)
	
			armature = node.data
			self.skeleton_info[node] = { "name":node.name, "bone_ids":{}, "has_transform":{}}
	
			for b in armature.bones:
				if (b.parent != None):
					# this node will be exported when the parent exports its children
					continue
				self.export_armature_bone(b, il, self.skeleton_info[node], lookup, nodes_lookup, parenting_map)
		
		finally:
			# restore original pose position setting
			if (pose_position != node.data.pose_position):
				node.data.pose_position = pose_position
				bpy.context.scene.update();							

	def export_cameras(self):
		cameras = 	set(
			map(lambda obj : obj.data,
			filter(lambda obj :	obj.type == "CAMERA", 	self.valid_nodes)
		));
			
		camera_lookup = {}
		for camera in cameras:
			camera_id = self.get_node_id(camera.name + '-camera');
			camera_lookup[camera] = camera_id
			self.export_camera(camera, camera_id)
			
		return camera_lookup
		
	def export_camera(self, camera, camera_id):

		self.writel(S_CAMS, 1, '<camera id="' + camera_id + '" name="' + camera.name + '">')
		self.writel(S_CAMS, 2, '<optics>')
		self.writel(S_CAMS, 3, '<technique_common>')
		if (camera.type == "PERSP"):
			self.writel(S_CAMS, 4, '<perspective>')
			self.writel(S_CAMS, 5, '<yfov> ' + str(math.degrees(camera.angle)) + ' </yfov>')  # I think?
			self.writel(S_CAMS, 5, '<aspect_ratio> ' + str(self.scene.render.resolution_x / self.scene.render.resolution_y) + ' </aspect_ratio>')
			self.writel(S_CAMS, 5, '<znear> ' + str(camera.clip_start) + ' </znear>')
			self.writel(S_CAMS, 5, '<zfar> ' + str(camera.clip_end) + ' </zfar>')
			self.writel(S_CAMS, 4, '</perspective>')
		else:
			self.writel(S_CAMS, 4, '<orthographic>')
			self.writel(S_CAMS, 5, '<xmag> ' + str(camera.ortho_scale * 0.5) + ' </xmag>')  # I think?
			self.writel(S_CAMS, 5, '<aspect_ratio> ' + str(self.scene.render.resolution_x / self.scene.render.resolution_y) + ' </aspect_ratio>')
			self.writel(S_CAMS, 5, '<znear> ' + str(camera.clip_start) + ' </znear>')
			self.writel(S_CAMS, 5, '<zfar> ' + str(camera.clip_end) + ' </zfar>')
			self.writel(S_CAMS, 4, '</orthographic>')

		self.writel(S_CAMS, 3, '</technique_common>')
		self.writel(S_CAMS, 2, '</optics>')
		self.writel(S_CAMS, 1, '</camera>')

	def export_lights(self):
		lights = set(
			map(lambda obj : obj.data,
			filter(lambda obj :	obj.type == "LAMP", 	self.valid_nodes)
		));
					
		light_lookup = {}
		for light in lights:
			light_id = self.get_node_id(light.name + '-light');
			light_lookup[light] = light_id
			self.export_lamp(light, light_id)
			
		return light_lookup
		
	def export_lamp(self, light, light_id):

		self.writel(S_LAMPS, 1, '<light id="' + light_id + '" name="' + light.name + '">')
		# self.writel(S_LAMPS,2,'<optics>')
		self.writel(S_LAMPS, 3, '<technique_common>')

		if (light.type == "POINT"):
			self.writel(S_LAMPS, 4, '<point>')
			self.writel(S_LAMPS, 5, '<color>' + strarr(light.color) + '</color>')
			att_by_distance = 2.0 / light.distance  # convert to linear attenuation
			self.writel(S_LAMPS, 5, '<linear_attenuation>' + str(att_by_distance) + '</linear_attenuation>')
			if (light.use_sphere):
				self.writel(S_LAMPS, 5, '<zfar>' + str(light.distance) + '</zfar>')
			self.writel(S_LAMPS, 4, '</point>')
		elif (light.type == "SPOT"):
			self.writel(S_LAMPS, 4, '<spot>')
			self.writel(S_LAMPS, 5, '<color>' + strarr(light.color) + '</color>')
			att_by_distance = 2.0 / light.distance  # convert to linear attenuation
			self.writel(S_LAMPS, 5, '<linear_attenuation>' + str(att_by_distance) + '</linear_attenuation>')
			self.writel(S_LAMPS, 5, '<falloff_angle>' + str(math.degrees(light.spot_size / 2)) + '</falloff_angle>')
			self.writel(S_LAMPS, 4, '</spot>')
		else:  # write a sun lamp for everything else (not supported)
			self.writel(S_LAMPS, 4, '<directional>')
			self.writel(S_LAMPS, 5, '<color>' + strarr(light.color) + '</color>')
			self.writel(S_LAMPS, 4, '</directional>')

		self.writel(S_LAMPS, 3, '</technique_common>')
		# self.writel(S_LAMPS,2,'</optics>')
		self.writel(S_LAMPS, 1, '</light>')

	def export_curve(self, curve, spline_id):

		self.writel(S_GEOM, 1, '<geometry id="' + spline_id + '" name="' + curve.name + '">')
		self.writel(S_GEOM, 2, '<spline closed="0">')

		points = []
		interps = []
		handles_in = []
		handles_out = []
		tilts = []

		for cs in curve.splines:
			if (cs.type == "BEZIER"):
				for s in cs.bezier_points:
					points.append(s.co[0])
					points.append(s.co[1])
					points.append(s.co[2])
					handles_in.append(s.handle_left[0])
					handles_in.append(s.handle_left[1])
					handles_in.append(s.handle_left[2])
					handles_out.append(s.handle_right[0])
					handles_out.append(s.handle_right[1])
					handles_out.append(s.handle_right[2])

					tilts.append(s.tilt)
					interps.append("BEZIER")
			else:

				for s in cs.points:
					points.append(s.co[0])
					points.append(s.co[1])
					points.append(s.co[2])
					handles_in.append(s.co[0])
					handles_in.append(s.co[1])
					handles_in.append(s.co[2])
					handles_out.append(s.co[0])
					handles_out.append(s.co[1])
					handles_out.append(s.co[2])
					tilts.append(s.tilt)
					interps.append("LINEAR")

		self.writel(S_GEOM, 3, '<source id="' + spline_id + '-positions">')
		position_values = " ".join([str(x) for x in points])
		self.writel(S_GEOM, 4, '<float_array id="' + spline_id + '-positions-array" count="' + str(len(points)) + '">' + position_values + '</float_array>')
		self.writel(S_GEOM, 4, '<technique_common>')
		self.writel(S_GEOM, 4, '<accessor source="#' + spline_id + '-positions-array" count="' + str(len(points) / 3) + '" stride="3">')
		self.writel(S_GEOM, 5, '<param name="X" type="float"/>')
		self.writel(S_GEOM, 5, '<param name="Y" type="float"/>')
		self.writel(S_GEOM, 5, '<param name="Z" type="float"/>')
		self.writel(S_GEOM, 4, '</accessor>')
		self.writel(S_GEOM, 4, '</technique_common>')
		self.writel(S_GEOM, 3, '</source>')
		self.writel(S_GEOM, 3, '<source id="' + spline_id + '-intangents">')
		intangent_values = " ".join([str(x) for x in handles_in])
		self.writel(S_GEOM, 4, '<float_array id="' + spline_id + '-intangents-array" count="' + str(len(points)) + '">' + intangent_values + '</float_array>')
		self.writel(S_GEOM, 4, '<technique_common>')
		self.writel(S_GEOM, 4, '<accessor source="#' + spline_id + '-intangents-array" count="' + str(len(points) / 3) + '" stride="3">')
		self.writel(S_GEOM, 5, '<param name="X" type="float"/>')
		self.writel(S_GEOM, 5, '<param name="Y" type="float"/>')
		self.writel(S_GEOM, 5, '<param name="Z" type="float"/>')
		self.writel(S_GEOM, 4, '</accessor>')
		self.writel(S_GEOM, 4, '</technique_common>')
		self.writel(S_GEOM, 3, '</source>')
		self.writel(S_GEOM, 3, '<source id="' + spline_id + '-outtangents">')
		outtangent_values = " ".join([str(x) for x in handles_out])
		self.writel(S_GEOM, 4, '<float_array id="' + spline_id + '-outtangents-array" count="' + str(len(points)) + '">' + outtangent_values + '</float_array>')
		self.writel(S_GEOM, 4, '<technique_common>')
		self.writel(S_GEOM, 4, '<accessor source="#' + spline_id + '-outtangents-array" count="' + str(len(points) / 3) + '" stride="3">')
		self.writel(S_GEOM, 5, '<param name="X" type="float"/>')
		self.writel(S_GEOM, 5, '<param name="Y" type="float"/>')
		self.writel(S_GEOM, 5, '<param name="Z" type="float"/>')
		self.writel(S_GEOM, 4, '</accessor>')
		self.writel(S_GEOM, 4, '</technique_common>')
		self.writel(S_GEOM, 3, '</source>')
		self.writel(S_GEOM, 3, '<source id="' + spline_id + '-interpolations">')
		interpolation_values = " ".join([str(x) for x in interps]) 
		self.writel(S_GEOM, 4, '<Name_array id="' + spline_id + '-interpolations-array" count="' + str(len(interps)) + '">' + interpolation_values + '</Name_array>')
		self.writel(S_GEOM, 4, '<technique_common>')
		self.writel(S_GEOM, 4, '<accessor source="#' + spline_id + '-interpolations-array" count="' + str(len(interps)) + '" stride="1">')
		self.writel(S_GEOM, 5, '<param name="INTERPOLATION" type="name"/>')
		self.writel(S_GEOM, 4, '</accessor>')
		self.writel(S_GEOM, 4, '</technique_common>')
		self.writel(S_GEOM, 3, '</source>')
		self.writel(S_GEOM, 3, '<source id="' + spline_id + '-tilts">')
		tilt_values = " ".join([str(x) for x in tilts]) 
		self.writel(S_GEOM, 4, '<float_array id="' + spline_id + '-tilts-array" count="' + str(len(tilts)) + '">' + tilt_values + '</float_array>')
		self.writel(S_GEOM, 4, '<technique_common>')
		self.writel(S_GEOM, 4, '<accessor source="#' + spline_id + '-tilts-array" count="' + str(len(tilts)) + '" stride="1">')
		self.writel(S_GEOM, 5, '<param name="TILT" type="float"/>')
		self.writel(S_GEOM, 4, '</accessor>')
		self.writel(S_GEOM, 4, '</technique_common>')
		self.writel(S_GEOM, 3, '</source>')
		self.writel(S_GEOM, 3, '<control_vertices>')
		self.writel(S_GEOM, 4, '<input semantic="POSITION" source="#' + spline_id + '-positions"/>')
		self.writel(S_GEOM, 4, '<input semantic="IN_TANGENT" source="#' + spline_id + '-intangents"/>')
		self.writel(S_GEOM, 4, '<input semantic="OUT_TANGENT" source="#' + spline_id + '-outtangents"/>')
		self.writel(S_GEOM, 4, '<input semantic="INTERPOLATION" source="#' + spline_id + '-interpolations"/>')
		self.writel(S_GEOM, 4, '<input semantic="TILT" source="#' + spline_id + '-tilts"/>')
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
			if (parent_bone.use_deform):
				parent = parent_bone
			else:
				parent = self.get_bone_deform_parent(parent_bone)
			if (not parent):
				matrix = node.matrix_local.copy()
			else:
				pose_parent = armature.pose.bones[parent.name]
				if self.is_zero_scale(pose_parent.matrix) or self.is_zero_scale(armature.matrix_world):
					visible = False
					matrix = Matrix()
				else:
					matrix = pose_parent.matrix.inverted() * (armature.matrix_world.inverted() * node.matrix_world)
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
			return {"matrix":matrix, "scale":scale}, visible
		else:
			return {"matrix":matrix}, visible

		
	def get_bone_deform_parent(self, bone):
		# traverse up bone parenting to get a parent bone that has deform checked
		parent = bone.parent
		while ((parent != None) and not parent.use_deform):
			parent = parent.parent
		return parent
	
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
			return {"matrix":matrix, "scale":(1.0, 1.0, 1.0)}
		else:
			return {"matrix":matrix}
			
	def get_posebone_transform(self, posebones_map, posebone):
		# get posebone transform relative to its parent bone, if no parent bone then relative to the armature
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
			return {"matrix":matrix, "scale":scale}
		else:
			return {"matrix":matrix}
		
	def get_scale_xml(self, scale):
		return ['<scale sid="scale">' + " ".join([str(e) for e in scale]) + '</scale>']
		
	def get_location_xml(self, location):
		return [('<translate sid="location">' + " ".join([str(e) for e in location]) + '</translate>')]
	
	def get_matrix_transform_xml(self, matrix):
		return['<matrix sid="transform">' + strmtx(matrix) + '</matrix>']
			
	def export_node(self, node, il, lookup, nodes_lookup):
		# export a scene node as a Collada node
		
		# The Blender scene tree suffers from a diamond problem when a node is parented by a bone, 
		# but also parented by the armature. So use the lookup to avoid exporting a node more than once.
		
		if ((not node in self.valid_nodes) or (node in nodes_lookup)):
			return

		node_id = self.get_node_id(node.name)
		nodes_lookup[node] = node_id
		
		self.writel(S_NODES, il, '<node id="' + node_id + '" name="' + node.name + '" type="NODE">')
		il += 1

		transforms = self.get_node_transform_xml(node)
		
		for t in transforms:
			self.writel(S_NODES, il, t)

		if (node.type == "ARMATURE"):
			lookup["Armature"] = node_id
			lookup["Armature_node"] = node
			self.export_armature_node(node, il, lookup, nodes_lookup)
		elif (node.data != None):
			if (node.data in lookup["skin_controller"]):
				count = 0
				for skin_lookup in lookup["skin_controller"][node.data]: 
					skin_id = skin_lookup['skin']
					skeleton = skin_lookup['skeleton']
					skin_sid = "skin" + str(count)
					self.writel(S_NODES, il, '<instance_controller url="#' + skin_id + '" sid="' + skin_sid + '">')
					self.writel(S_NODES, il + 1, '<skeleton>#' + skeleton + '</skeleton>')
					self.export_material_bind(node, il, lookup)
					self.writel(S_NODES, il, "</instance_controller>")
					count += 1
			elif (node.data in lookup["geometry_morphs"]):
				morph_id = lookup["morph"][node.data][0]
				morph_sid = "morph"
				self.writel(S_NODES, il, '<instance_controller url="#' + morph_id + '" sid="' + morph_sid + '">')
				self.export_material_bind(node, il, lookup)
				self.writel(S_NODES, il, "</instance_controller>")
			elif (node.data in lookup["mesh"]):
				mesh_id = lookup["mesh"][node.data]
				self.writel(S_NODES, il, '<instance_geometry url="#' + mesh_id + '">')
				self.export_material_bind(node, il, lookup)
				self.writel(S_NODES, il, "</instance_geometry>")
			elif (node.data in lookup["camera"]):
				camera_id = lookup["camera"][node.data]
				self.writel(S_NODES, il, '<instance_camera url="#' + camera_id + '"/>')
			elif (node.data in lookup["light"]):
				light_id = lookup["light"][node.data]
				self.writel(S_NODES, il, '<instance_light url="#' + light_id + '"/>')
		
		for x in node.children:
			self.export_node(x, il, lookup, nodes_lookup)

		il -= 1
		self.writel(S_NODES, il, '</node>')

	def get_armature_children_of_bones(self, armature_node):
		# get the scene nodes that are attached to the armature through bone parenting
		
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
		if (not node.type in self.config["object_types"]):
			return False
		if (self.config["use_active_layers"]):
			valid = False
			# print("NAME: "+node.name)
			for i in range(20):
				if (node.layers[i] and self.scene.layers[i]):
					valid = True
					break
			if (not valid):
				return False
		if (self.config["use_export_selected"] and not node.select):
			return False
		return True

	def export_material_bind(self, node, il, lookup):
		if not node.material_slots:
			return
		if not len(node.material_slots):
			return
		material_bind = lookup["material_bind"].get(node.data, None)
		if not material_bind:
			return
		
		self.writel(S_NODES, il + 1, '<bind_material>')
		self.writel(S_NODES, il + 2, '<technique_common>')
		for material_slot, material_symbol in material_bind.items():
			material = node.material_slots[material_slot].material
			if material:
				material_id = lookup["material"][material]
				if material_id:
					self.writel(S_NODES, il + 3, '<instance_material symbol="' + material_symbol + '" target="#' + material_id + '"/>')
		self.writel(S_NODES, il + 2, '</technique_common>')
		self.writel(S_NODES, il + 1, '</bind_material>')

	def export_materials(self):

		# get materials for export
		
		if (self.config["use_export_selected"]):
			# only export used materials for meshes that will be exported
			materials = 	set(
			filter(lambda mat : mat != None,
			itertools.chain.from_iterable(
				filter(lambda values : values != None,
				map(lambda obj :	obj.data.materials,
				filter(lambda node : hasattr(node.data, "materials"),
					self.valid_nodes)
			)))));
		else:
			# Export all materials including those with fake users
			materials = set([material for material in bpy.data.materials if material.users > 0])
				
	
		# get texture map images
	
		if (self.config["use_export_selected"]):
			# Textures based selected materials
			images = set(
				map(lambda ts : ts.texture.image,
			 filter(
					lambda ts : ts and ts.use and ts.texture and ts.texture.type == "IMAGE" and ts.texture.image,
					itertools.chain.from_iterable(
					filter(lambda values : values != None,
					map(lambda material : material.texture_slots.values(), materials)
					)))));
		else:
			# All textures including fake user and unused
			images = set(
						map(lambda texture : texture.image,
						filter(
							lambda texture : texture and texture.type == "IMAGE" and texture.image,
							bpy.data.textures.values()
						)))
			
		# export library_images content
			
		image_lookup = {}
		for image in images:
			image_id = self.get_node_id(image.name + "-image")
			image_lookup[image] = image_id
			self.export_image(image, image_id)
				
		
		# export library_effects content
		
		effect_lookup = {}				
		for mat in materials:
			effect_id = self.get_node_id(mat.name + "-effect")
			effect_lookup[mat] = effect_id
			self.export_effect(mat, effect_id, image_lookup)
		
		# export library_materials content
		
		material_lookup = {}
		for mat in materials:
			material_id = self.get_node_id(mat.name + "-material")
			material_lookup[mat] = material_id
			self.export_material(mat, material_id, effect_lookup[mat])
			
		return material_lookup
			
	def export_material(self, material, material_id, effect_id):
		# Material
		self.writel(S_MATS, 1, '<material id="' + material_id + '" name="' + material.name + '">')
		self.writel(S_MATS, 2, '<instance_effect url="#' + effect_id + '"/>')
		self.writel(S_MATS, 1, '</material>')

	def get_valid_nodes(self):
				# validate nodes
		for obj in self.scene.objects:
			if (obj in self.valid_nodes):
				continue
			if (self.is_node_valid(obj)):
				n = obj
				while (n != None):
					if (not n in self.valid_nodes):
						self.valid_nodes.append(n)
					n = n.parent

	def export_physics_materials(self):
		collision_nodes = [node for node in self.valid_nodes if (node.rigid_body != None)]
		if (not len(collision_nodes)):
			return {}
		physics_materials_lookup = {}
		self.writel(S_P_MATS, 0, '<library_physics_materials>')
		for node in collision_nodes:
			if (not node.data in physics_materials_lookup):
				physics_material_id = self.get_node_id(node.data.name + '-phys_mat')
				physics_materials_lookup[node.data] = physics_material_id
				self.export_physics_material(node, physics_material_id)
		self.writel(S_P_MATS, 0, '</library_physics_materials>')
		return physics_materials_lookup
	
	def export_physics_material(self, node, physics_material_id):
		self.writel(S_P_MATS, 1, '<physics_material id ="{}">'.format(physics_material_id))
		self.writel(S_P_MATS, 2, '<technique_common>')
		self.writel(S_P_MATS, 3, '<dynamic_friction>{}</dynamic_friction>'.format(node.rigid_body.friction))
		self.writel(S_P_MATS, 3, '<static_friction>{}</static_friction>'.format(node.rigid_body.friction))
		self.writel(S_P_MATS, 3, '<restitution>{}</restitution>'.format(node.rigid_body.restitution))
		self.writel(S_P_MATS, 2, '</technique_common>')
		self.writel(S_P_MATS, 1, '</physics_material>')
		
	def export_physics_models(self, lookup):
		collision_shape_nodes = [node for node in self.valid_nodes if ((node.rigid_body != None) and (node.rigid_body.collision_shape != None))]
		if (not len(collision_shape_nodes)):
			return
		
		physics_model_lookup = {}
		self.writel(S_P_MODEL, 0, '<library_physics_models>')
		for node in collision_shape_nodes:
			if (not node.data in physics_model_lookup):
				physics_model_id = self.get_node_id(node.data.name + '-model')
				physics_model_lookup[node.data] = physics_model_id
				self.export_physics_model(node, physics_model_id, lookup)
		self.writel(S_P_MODEL, 0, '</library_physics_models>')
		return physics_model_lookup
	
	def export_physics_model(self, node, physics_model_id, lookup):
		self.writel(S_P_MODEL, 1, '<physics_model id="{}">'.format(physics_model_id))
		self.writel(S_P_MODEL, 2, '<rigid_body sid="{}">'.format(lookup['nodes'][node]))
		self.writel(S_P_MODEL, 3, '<technique_common>')
		self.writel(S_P_MODEL, 4, '<dynamic>{}</dynamic>'.format(str(node.rigid_body.enabled).lower()))
		self.writel(S_P_MODEL, 4, '<mass>{}</mass>'.format(node.rigid_body.mass))
		self.writel(S_P_MODEL, 4, '<mass_frame>')
		self.writel(S_P_MODEL, 5, '<translate>0 0 0</translate>')
		self.writel(S_P_MODEL, 5, '<rotate>0 0 1 0</rotate>')
		self.writel(S_P_MODEL, 4, '</mass_frame>')
		self.writel(S_P_MODEL, 4, '<shape>')
		try:  # in case no physics material in the lookup
			self.writel(S_P_MODEL, 5, '<instance_physics_material url="#{}"/>'.format(lookup['physics_material'][node.data]))
		except:
			pass  # don't need it then
		shape_funcs = {
					'BOX': self.export_box_shape,
					'SPHERE': self.export_sphere_shape,
					'CAPSULE': self.export_capsule_shape,
					'CYLINDER': self.export_cylinder_shape,
					'CONE': self.export_cone_shape,
					'CONVEX_HULL': self.export_convex_hull_shape,
					'MESH': self.export_mesh_shape}
		try:
			shape_funcs[node.rigid_body.collision_shape](node, 5, lookup)
		except:
			pass
		self.export_collision_margin(node, 5)

		self.writel(S_P_MODEL, 4, '</shape>')
		collision_groups = [str(i) for (i, g) in enumerate(node.rigid_body.collision_groups) if g]
		self.writel(S_P_MODEL, 3, '</technique_common>')
		
		self.writel(S_P_MODEL, 3, '<extra>')
		self.writel(S_P_MODEL, 4, '<technique profile="blender">')
		self.writel(S_P_MODEL, 5, '<linear_damping>{}</linear_damping>'.format(node.rigid_body.linear_damping))
		self.writel(S_P_MODEL, 5, '<angular_damping>{}</angular_damping>'.format(node.rigid_body.angular_damping))
		if(node.rigid_body.use_deactivation):
			self.writel(S_P_MODEL, 5, '<deactivation>')
			self.writel(S_P_MODEL, 6, '<linear_velocity>{}</linear_velocity>'.format(node.rigid_body.deactivate_linear_velocity))
			self.writel(S_P_MODEL, 6, '<angular_velocity>{}</angular_velocity>'.format(node.rigid_body.deactivate_angular_velocity))
			self.writel(S_P_MODEL, 6, '<start>{}</start>'.format(str(node.rigid_body.use_start_deactivated).lower()))
			self.writel(S_P_MODEL, 5, '</deactivation>')
			
		if (len(collision_groups)):
			self.writel(S_P_MODEL, 5, '<collision_groups>{}</collision_groups>'.format(" ".join(collision_groups)))

		self.writel(S_P_MODEL, 4, '</technique>')
		self.writel(S_P_MODEL, 3, '</extra>')
		self.writel(S_P_MODEL, 2, '</rigid_body>')
		self.writel(S_P_MODEL, 1, '</physics_model>')

	def get_node_dimensions(self, node):
		# get dimensions before scaling
		return [d / s for (d, s)in zip(node.dimensions, node.scale)]
	
	def export_collision_margin(self, node, il):
		if (node.rigid_body.use_margin or node.rigid_body.collision_shape == 'CONE'):
			self.writel(
					S_P_MODEL, il,
					'<collision_margin>{}</collision_margin>'
					.format(node.rigid_body.collision_margin))
				
	def export_box_shape(self, node, il, lookup):
		dimensions = self.get_node_dimensions(node)
		self.writel(S_P_MODEL, il, "<box>")
		self.writel(S_P_MODEL, il + 1, "<half_extents>{} {} {}</half_extents>".format(
			dimensions[0] / 2.0, dimensions[1] / 2.0, dimensions[2] / 2.0))
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
		self.writel(S_P_MODEL, il + 1, "<height>{}</height>".format(dimensions[2]))
		radius = max(dimensions[0], dimensions[1]) / 2.0
		self.writel(S_P_MODEL, il + 1, "<radius>{} {} {}</radius>".format(radius, radius, radius))
		self.writel(S_P_MODEL, il, "</capsule>")	

	def export_cylinder_shape(self, node, il, lookup):
		dimensions = self.get_node_dimensions(node)
		self.writel(S_P_MODEL, il, "<cylinder>")
		self.writel(S_P_MODEL, il + 1, "<height>{}</height>".format(dimensions[2]))
		radius = max(dimensions[0], dimensions[1]) / 2.0
		self.writel(S_P_MODEL, il + 1, "<radius>{} {}</radius>".format(radius, radius))
		self.writel(S_P_MODEL, il, "</cylinder>")	
		
	def export_cone_shape(self, node, il, lookup):
		dimensions = self.get_node_dimensions(node)
		self.writel(S_P_MODEL, il, "<cone>")
		self.writel(S_P_MODEL, il + 1, "<height>{}</height>".format(dimensions[2]))
		radius = max(dimensions[0], dimensions[1]) / 2.0
		self.writel(S_P_MODEL, il + 1, "<radius>{} {}</radius>".format(radius, radius))
		self.writel(S_P_MODEL, il, "</cone>")	

	def get_physics_mesh_id(self, node, lookup):
		
		# get the source for building a physics mesh based on the mesh_source setting
		try:
			if (self.node_has_convex_hull(node)):
				return True, lookup['convex_mesh'][node.data]
			else:
				return False, lookup['nodes'][node]
		except:
			return False, None
		
	def export_convex_hull_shape(self, node, il, lookup):
		convex, mesh_id = self.get_physics_mesh_id(node, lookup)
		if (mesh_id != None):
			if (convex):
				self.writel(S_P_MODEL, il, '<instance_geometry url="#{}"/>'.format(mesh_id))
			else:
				self.writel(S_P_MODEL, il, '<convex_mesh convex_hull_of="#{}"/>'.format(mesh_id))
		
	def export_mesh_shape(self, node, il, lookup):
		mesh_id = self.get_physics_mesh_id(node, lookup)
		if (mesh_id != None):
			self.writel(S_P_MODEL, il, '<instance_geometry url="#{}">'.format(mesh_id))
		
	def export_physics_scene(self, lookup):
		if (bpy.context.scene.rigidbody_world == None):
			return
		
		self.writel(S_P_SCENE, 0, '<library_physics_scenes>')
		self.writel(S_P_SCENE, 1, '<physics_scene id="{}">'.format(self.scene_name + '-physics'))
		self.writel(S_P_SCENE, 2, '<technique_common>')
		self.writel(S_P_SCENE, 3, '<gravity>{}</gravity>'.format(" ".join([str(e) for e in bpy.context.scene.gravity])))
		self.writel(S_P_SCENE, 3, '<time_step>{}</time_step>'.format(1.0 / bpy.context.scene.rigidbody_world.steps_per_second))
		self.writel(S_P_SCENE, 2, '</technique_common>')
		self.writel(S_P_SCENE, 2, '<extra>')
		self.writel(S_P_SCENE, 3, '<technique profile="blender">')
		self.writel(S_P_SCENE, 4, '<time_scale>{}</time_scale>'.format(bpy.context.scene.rigidbody_world.time_scale))
		self.writel(S_P_SCENE, 4, '<solver_iterations>{}</solver_iterations>'.format(bpy.context.scene.rigidbody_world.solver_iterations))
		self.writel(S_P_SCENE, 4, '<split_impulse>{}</split_impulse>'.format(str(bpy.context.scene.rigidbody_world.use_split_impulse).lower()))
		self.writel(S_P_SCENE, 4, '<weights>')
		self.writel(S_P_SCENE, 5, '<all>{}</all>'.format(bpy.context.scene.rigidbody_world.effector_weights.all))
		self.writel(S_P_SCENE, 5, '<gravity>{}</gravity>'.format(bpy.context.scene.rigidbody_world.effector_weights.gravity))
		self.writel(S_P_SCENE, 5, '<force>{}</force>'.format(bpy.context.scene.rigidbody_world.effector_weights.force))
		self.writel(S_P_SCENE, 5, '<harmonic>{}</harmonic>'.format(bpy.context.scene.rigidbody_world.effector_weights.harmonic))
		self.writel(S_P_SCENE, 5, '<vortex>{}</vortex>'.format(bpy.context.scene.rigidbody_world.effector_weights.vortex))
		self.writel(S_P_SCENE, 5, '<charge>{}</charge>'.format(bpy.context.scene.rigidbody_world.effector_weights.charge))
		self.writel(S_P_SCENE, 5, '<magnetic>{}</magnetic>'.format(bpy.context.scene.rigidbody_world.effector_weights.magnetic))
		self.writel(S_P_SCENE, 5, '<lennardjones>{}</lennardjones>'.format(bpy.context.scene.rigidbody_world.effector_weights.lennardjones))
		self.writel(S_P_SCENE, 5, '<wind>{}</wind>'.format(bpy.context.scene.rigidbody_world.effector_weights.wind))
		self.writel(S_P_SCENE, 5, '<turbulence>{}</turbulence>'.format(bpy.context.scene.rigidbody_world.effector_weights.turbulence))
		self.writel(S_P_SCENE, 5, '<curve_guide>{}</curve_guide>'.format(bpy.context.scene.rigidbody_world.effector_weights.curve_guide))
		self.writel(S_P_SCENE, 5, '<drag>{}</drag>'.format(bpy.context.scene.rigidbody_world.effector_weights.drag))
		self.writel(S_P_SCENE, 5, '<texture>{}</texture>'.format(bpy.context.scene.rigidbody_world.effector_weights.texture))
		self.writel(S_P_SCENE, 5, '<boid>{}</boid>'.format(bpy.context.scene.rigidbody_world.effector_weights.boid))
		self.writel(S_P_SCENE, 5, '<smokeflow>{}</smokeflow>'.format(bpy.context.scene.rigidbody_world.effector_weights.smokeflow))
		self.writel(S_P_SCENE, 4, '</weights>')
		self.writel(S_P_SCENE, 3, '</technique>')
		self.writel(S_P_SCENE, 2, '</extra>')
		self.writel(S_P_SCENE, 1, '</physics_scene>')
		self.writel(S_P_SCENE, 0, '</library_physics_scenes>')
		
	def export_scene(self, lookup):

		nodes_lookup = {}
				
		self.writel(S_NODES, 0, '<library_visual_scenes>')
		self.writel(S_NODES, 1, '<visual_scene id="' + self.scene_name + '" name="' + self.scene_name + '">')

		for obj in self.valid_nodes:
			if (obj.parent == None):
				self.export_node(obj, 2, lookup, nodes_lookup)

		self.writel(S_NODES, 1, '</visual_scene>')
		self.writel(S_NODES, 0, '</library_visual_scenes>')
		return nodes_lookup
	
	def export_asset(self):
		self.writel(S_ASSET, 0, '<asset>')
		self.writel(S_ASSET, 1, '<contributor>')
		self.writel(S_ASSET, 2, '<author> Anonymous </author>') 
		self.writel(S_ASSET, 2, '<authoring_tool> Collada Exporter for Blender 2.6+, by Gregery Barton (gregery20@yahoo.com.au) </authoring_tool>') 
		self.writel(S_ASSET, 1, '</contributor>')
		self.writel(S_ASSET, 1, '<created>' + time.strftime("%Y-%m-%dT%H:%M:%SZ     ") + '</created>')
		self.writel(S_ASSET, 1, '<modified>' + time.strftime("%Y-%m-%dT%H:%M:%SZ") + '</modified>')
		self.writel(S_ASSET, 1, '<unit meter="1.0" name="meter"/>')
		self.writel(S_ASSET, 1, '<up_axis>Z_UP</up_axis>')
		self.writel(S_ASSET, 0, '</asset>')

	def export_animation_blends(self, target, action_name, keys):

		frame_total = len(keys)
		if (action_name == None):
			anim_id = target + "-anim"
		else:
			anim_id = action_name + "-" + target + "-anim"
		anim_id = self.get_node_id(anim_id.replace("/", "-"))
		
		self.writel(S_ANIM, 1, '<animation id="' + anim_id + '">')

		source_frames = " ".join([str(k[0]) for k in keys])
		source_value = " ".join([str(k[1]) for k in keys])

		source_interps = " ".join([(" LINEAR ") * len(keys)]) 

		# Time Source
		self.writel(S_ANIM, 2, '<source id="' + anim_id + '-input">')
		self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-input-array" count="' + str(frame_total) + '">' + source_frames + '</float_array>')
		self.writel(S_ANIM, 3, '<technique_common>')
		self.writel(S_ANIM, 4, '<accessor source="#' + anim_id + '-input-array" count="' + str(frame_total) + '" stride="1">')
		self.writel(S_ANIM, 5, '<param name="TIME" type="float"/>')
		self.writel(S_ANIM, 4, '</accessor>')
		self.writel(S_ANIM, 3, '</technique_common>')
		self.writel(S_ANIM, 2, '</source>')

		# Value Source
		self.writel(S_ANIM, 2, '<source id="' + anim_id + '-weights-output">')
		self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-weights-output-array" count="' + str(frame_total) + '">' + source_value + '</float_array>')
		self.writel(S_ANIM, 3, '<technique_common>')
		self.writel(S_ANIM, 4, '<accessor source="#' + anim_id + '-weights-output-array" count="' + str(frame_total) + '" stride="1">')
		self.writel(S_ANIM, 5, '<param name="X" type="float"/>')
		self.writel(S_ANIM, 4, '</accessor>')
		self.writel(S_ANIM, 3, '</technique_common>')
		self.writel(S_ANIM, 2, '</source>')

		# Interpolation Source
		self.writel(S_ANIM, 2, '<source id="' + anim_id + '-interpolation-output">')
		self.writel(S_ANIM, 3, '<Name_array id="' + anim_id + '-interpolation-output-array" count="' + str(frame_total) + '">' + source_interps + '</Name_array>')
		self.writel(S_ANIM, 3, '<technique_common>')
		self.writel(S_ANIM, 4, '<accessor source="#' + anim_id + '-interpolation-output-array" count="' + str(frame_total) + '" stride="1">')
		self.writel(S_ANIM, 5, '<param name="INTERPOLATION" type="Name"/>')
		self.writel(S_ANIM, 4, '</accessor>')
		self.writel(S_ANIM, 3, '</technique_common>')
		self.writel(S_ANIM, 2, '</source>')

		self.writel(S_ANIM, 2, '<sampler id="' + anim_id + '-weights-sampler">')
		self.writel(S_ANIM, 3, '<input semantic="INPUT" source="#' + anim_id + '-input"/>')
		self.writel(S_ANIM, 3, '<input semantic="OUTPUT" source="#' + anim_id + '-weights-output"/>')
		self.writel(S_ANIM, 3, '<input semantic="INTERPOLATION" source="#' + anim_id + '-interpolation-output"/>')
		self.writel(S_ANIM, 2, '</sampler>')

		self.writel(S_ANIM, 2, '<channel source="#' + anim_id + '-weights-sampler" target="' + target + '"/>')
		self.writel(S_ANIM, 1, '</animation>')

		return anim_id

	def export_animation_xforms(self, target, action_name, keys):

		frame_total = len(keys)
		if (action_name == None):
			anim_id = target + "-anim"
		else:
			anim_id = action_name + "-" + target + "-anim"
			
		self.writel(S_ANIM, 1, '<animation id="' + anim_id + '">')

		source_frames = " ".join([str(k[0]) for k in keys])
		source_matrix = " ".join([strmtx(k[1]['matrix']) for k in keys])
		if (self.transform_matrix_scale):
			source_scale = " ".join([str(e) for v in [k[1]['scale'] for k in keys] for e in v])

		source_interps = " ".join([(" LINEAR ") * len(keys)]) 

		# Time Source
		self.writel(S_ANIM, 2, '<source id="' + anim_id + '-input">')
		self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-input-array" count="' + str(frame_total) + '">' + source_frames + '</float_array>')
		self.writel(S_ANIM, 3, '<technique_common>')
		self.writel(S_ANIM, 4, '<accessor source="#' + anim_id + '-input-array" count="' + str(frame_total) + '" stride="1">')
		self.writel(S_ANIM, 5, '<param name="TIME" type="float"/>')
		self.writel(S_ANIM, 4, '</accessor>')
		self.writel(S_ANIM, 3, '</technique_common>')
		self.writel(S_ANIM, 2, '</source>')

		# Transform Source
		self.writel(S_ANIM, 2, '<source id="' + anim_id + '-matrix-output">')
		self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-matrix-output-array" count="' + str(frame_total * 16) + '">' + source_matrix + '</float_array>')
		self.writel(S_ANIM, 3, '<technique_common>')
		self.writel(S_ANIM, 4, '<accessor source="#' + anim_id + '-matrix-output-array" count="' + str(frame_total) + '" stride="16">')
		self.writel(S_ANIM, 5, '<param name="TRANSFORM" type="float4x4"/>')
		self.writel(S_ANIM, 4, '</accessor>')
		self.writel(S_ANIM, 3, '</technique_common>')
		self.writel(S_ANIM, 2, '</source>')
		
		# Scale Source
		if (self.transform_matrix_scale):
			self.writel(S_ANIM, 2, '<source id="' + anim_id + '-scale-output">')
			self.writel(S_ANIM, 3, '<float_array id="' + anim_id + '-scale-output-array" count="' + str(frame_total * 3) + '">' + source_scale + '</float_array>')
			self.writel(S_ANIM, 3, '<technique_common>')
			self.writel(S_ANIM, 4, '<accessor source="#' + anim_id + '-scale-output-array" count="' + str(frame_total) + '" stride="3">')
			self.writel(S_ANIM, 5, '<param name="X" type="float"/>')
			self.writel(S_ANIM, 5, '<param name="Y" type="float"/>')
			self.writel(S_ANIM, 5, '<param name="Z" type="float"/>')
			self.writel(S_ANIM, 4, '</accessor>')
			self.writel(S_ANIM, 3, '</technique_common>')
			self.writel(S_ANIM, 2, '</source>')

		# Interpolation Source
		self.writel(S_ANIM, 2, '<source id="' + anim_id + '-interpolation-output">')
		self.writel(S_ANIM, 3, '<Name_array id="' + anim_id + '-interpolation-output-array" count="' + str(frame_total) + '">' + source_interps + '</Name_array>')
		self.writel(S_ANIM, 3, '<technique_common>')
		self.writel(S_ANIM, 4, '<accessor source="#' + anim_id + '-interpolation-output-array" count="' + str(frame_total) + '" stride="1">')
		self.writel(S_ANIM, 5, '<param name="INTERPOLATION" type="Name"/>')
		self.writel(S_ANIM, 4, '</accessor>')
		self.writel(S_ANIM, 3, '</technique_common>')
		self.writel(S_ANIM, 2, '</source>')

		self.writel(S_ANIM, 2, '<sampler id="' + anim_id + '-matrix-sampler">')
		self.writel(S_ANIM, 3, '<input semantic="INPUT" source="#' + anim_id + '-input"/>')
		self.writel(S_ANIM, 3, '<input semantic="OUTPUT" source="#' + anim_id + '-matrix-output"/>')
		self.writel(S_ANIM, 3, '<input semantic="INTERPOLATION" source="#' + anim_id + '-interpolation-output"/>')
		self.writel(S_ANIM, 2, '</sampler>')

		if (self.transform_matrix_scale):
			self.writel(S_ANIM, 2, '<sampler id="' + anim_id + '-scale-sampler">')
			self.writel(S_ANIM, 3, '<input semantic="INPUT" source="#' + anim_id + '-input"/>')
			self.writel(S_ANIM, 3, '<input semantic="OUTPUT" source="#' + anim_id + '-scale-output"/>')
			self.writel(S_ANIM, 3, '<input semantic="INTERPOLATION" source="#' + anim_id + '-interpolation-output"/>')
			self.writel(S_ANIM, 2, '</sampler>')
		
		self.writel(S_ANIM, 2, '<channel source="#' + anim_id + '-matrix-sampler" target="' + target + '/transform"/>')
		if (self.transform_matrix_scale):
			self.writel(S_ANIM, 2, '<channel source="#' + anim_id + '-scale-sampler" target="' + target + '/scale"/>')
		self.writel(S_ANIM, 1, '</animation>')

		return anim_id

	def get_animation_transforms(self, start, end, lookup):
		
		# Blender -> Collada frames needs a little work
		# Collada starts from 0, blender usually from 1
		# The last frame must be included also

		frame_orig = self.scene.frame_current

		frame_len = 1.0 / self.scene.render.fps

		xform_cache = {}
		blend_cache = {}
		# Change frames first, export objects last
		# This improves performance enormously

		for t in range(start, end + 1):
			self.scene.frame_set(t)
			
			key = (t - 1) * frame_len

			for node in self.scene.objects:

				if (not node in self.valid_nodes):
					continue
				if (node.data and (node.data in lookup["morph"])):
					morph_controller = lookup["morph"][node.data]
					morph_id = morph_controller[0]
					targets = morph_controller[1]
					for i in range(1, len(node.data.shape_keys.key_blocks)):

						name = morph_id + "/" + targets[i - 1][0]
						if (not (name in blend_cache)):
							blend_cache[name] = []

						self.append_morph_keyframe_if_different(blend_cache[name], key, node.data.shape_keys.key_blocks[i].value)


				if (node.type == "ARMATURE"):
					# All bones exported for now

					for bone in node.data.bones:

						bone_node_id = self.skeleton_info[node]["bone_ids"].get(bone, None)
						if (bone_node_id is None or not self.skeleton_info[node]["has_transform"][bone]):
							continue

						posebone = node.pose.bones[bone.name]
						transform = self.get_posebone_transform(node.pose.bones, posebone)
						if transform:
							if (not (bone_node_id in xform_cache)):
								xform_cache[bone_node_id] = []
							self.append_keyframe_if_different(xform_cache[bone_node_id], transform, key)

				transform, visible = self.get_node_local_transform(node)
				if visible:
					node_id = lookup["nodes"][node]
					if (not (node_id in xform_cache)):
						xform_cache[node_id] = []
					self.append_keyframe_if_different(xform_cache[node_id], transform, key)

		self.scene.frame_set(frame_orig)

		xform_cache = self.xform_cache_without_constants(xform_cache)
		blend_cache = self.blend_cache_without_constants(blend_cache)
		
		return xform_cache, blend_cache
	
	def append_morph_keyframe_if_different(self, morph_keyframes, new_key, new_value):
		different = False
		if (len(morph_keyframes)):
			prev_value = morph_keyframes[-1][1];
			if (abs(prev_value - new_value) > CMP_EPSILON):
				different = True
		else:
			different = True
		
		if different:
			morph_keyframes.append((new_key, new_value))
	
	def blend_cache_without_constants(self, blend_cache):
		# remove morph animations that only have one weight value
		blend_result = {}
		for name, keyframes in blend_cache.items():
			if (len(keyframes) > 1):
				blend_result[name] = keyframes
				
		return blend_result
			
	def xform_cache_without_constants(self, xform_cache):
		# remove fcurves that have only one transformation over the entire timeline
		xform_result = {}
		for name, transforms in xform_cache.items():
			if (len(transforms) > 1):
				xform_result[name] = transforms
		return xform_result
	
	def is_zero_scale(self, matrix):
		if (matrix[0][0] == 0.0) and (matrix[1][1] == 0) and (matrix[2][2] == 0):
			return True
		else:
			return False
		
	def append_keyframe_if_different(self, transforms, new_transform, new_key):
		different = False

		if (len(transforms)):
			prev_transform = transforms[-1][1];
			prev_matrix = prev_transform.get("matrix")
			new_matrix = new_transform.get("matrix")
			if (prev_matrix and new_matrix):
				same_matrix = matrix_equal(prev_matrix, new_matrix)
			else:
				same_matrix = True
			
			different = not same_matrix
			
			if (not different):
				prev_scale	 = prev_transform.get("scale")
				new_scale = new_transform.get("scale")
				if (prev_scale and new_scale):
					same_scale = vector_equal(prev_scale, new_scale)
				else:
					same_scale = True
				different = not same_scale
		else:
			different = True
		
		if (different):
			transforms.append((new_key, new_transform))
			
	def get_NLA_objects(self):
		objects = {}
		
		for node in self.scene.objects:
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
		self.writel(S_ANIM_CLIPS, 1, '<animation_clip id="' + id + '" start="' + str((start - 1) / self.scene.render.fps) + '" end="' + str((end - 1) / self.scene.render.fps) + '">')
		for z in tcn:
			self.writel(S_ANIM_CLIPS, 2, '<instance_animation url="#' + z + '"/>')
		self.writel(S_ANIM_CLIPS, 1, '</animation_clip>')


	def export_timeline(self, action_name, start, end, lookup):
		xform_cache, blend_cache = self.get_animation_transforms(start, end, lookup)
		tcn = []
		for node_id, cache in xform_cache.items():
			tcn.append(self.export_animation_xforms(node_id, action_name, cache))
		for target_id, cache in blend_cache.items():
			tcn.append(self.export_animation_blends(target_id, action_name, cache))
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
		start = sys.float_info.max;
		end = -sys.float_info.max;
		for track in node.animation_data.nla_tracks:
			for strip in track.strips:
				if start > strip.frame_start:
					start = strip.frame_start
				if end < strip.frame_end:
					end = strip.frame_end
		
		return int(start), int(end)
		

	def get_NLA_track_timeline(self, track):
		start = sys.float_info.max;
		end = -sys.float_info.max;
		for strip in track.strips:
			if start > strip.frame_start:
				start = strip.frame_start
			if end < strip.frame_end:
				end = strip.frame_end
		
		return int(start), int(end)
	
	def export_animations(self, lookup):
		
		self.writel(S_ANIM, 0, '<library_animations>')
		self.writel(S_ANIM_CLIPS, 0, '<library_animation_clips>')

		if (self.config["use_anim_timeline"]):
			self.export_timeline("timeline", self.scene.frame_start, self.scene.frame_end, lookup)

		if self.config["clip_type"] != 'NONE':
			nla = self.get_NLA_objects()

			if self.config["clip_type"] == 'OBJECT':
				for node in nla.keys():
					self.mute_NLA(nla)
					self.unmute_NLA_object(node)
					start, end = self.get_NLA_object_timeline(node)
					self.export_timeline(self.get_node_id(node.name), start, end, lookup)
			
			if self.config["clip_type"] == 'TRACK':
				for tracks in nla.values():
					for track in tracks:
						self.mute_NLA(nla)
						self.unmute_NLA_track(track[0])
						start, end = self.get_NLA_track_timeline(track[0])
						self.export_timeline(self.get_node_id(track[0].name), start, end, lookup)
						
			if self.config["clip_type"] == 'STRIP':
				for tracks in nla.values():
					for track in tracks:
						for strip in track[2]:
							strip[0].mute = False
							start = int(strip[0].frame_start)
							end = int(strip[0].frame_end)
							self.export_timeline(self.get_node_id(strip[0].name), start, end, lookup)
							strip[0].mute = True
			
			
			self.restore_NLA(nla)

		self.writel(S_ANIM_CLIPS, 0, '</library_animation_clips>')
		self.writel(S_ANIM, 0, '</library_animations>')

	def remove_export_meshes(self):
		for me in self.meshes_to_clear:
			me.free_normals_split()
			bpy.data.meshes.remove(me)
		self.meshes_to_clear = []

	def export(self):
		try:
			self.writel(S_GEOM, 0, '<library_geometries>')
			self.writel(S_CONT, 0, '<library_controllers>')
			self.writel(S_CAMS, 0, '<library_cameras>')
			self.writel(S_LAMPS, 0, '<library_lights>')
			self.writel(S_IMGS, 0, '<library_images>')
			self.writel(S_MATS, 0, '<library_materials>')
			self.writel(S_FX, 0, '<library_effects>')

			self.get_valid_nodes()
			self.skeletons = []
			self.export_asset()
			material_lookup = self.export_materials()
			mesh_lookup, convex_mesh_lookup, geometry_morphs, material_bind_lookup = self.export_meshes()
			camera_lookup = self.export_cameras()
			light_lookup = self.export_lights()
			morph_lookup = self.export_morph_controllers(mesh_lookup, geometry_morphs)
			skin_controller_lookup = self.export_skin_controllers(mesh_lookup, morph_lookup)
			lookup = {
				"material":material_lookup,
				"mesh":mesh_lookup,
				"convex_mesh":convex_mesh_lookup,
				"geometry_morphs":geometry_morphs,
				"camera":camera_lookup,
				"light":light_lookup,
				"morph":morph_lookup,
				"skin_controller":skin_controller_lookup,
				"material_bind": material_bind_lookup}
			
			nodes_lookup = self.export_scene(lookup)
			lookup["nodes"] = nodes_lookup
			
			if (self.config["use_physics"]):
				physics_material_lookup = self.export_physics_materials()
				lookup["physics_material"] = physics_material_lookup
				physics_model_lookup = self.export_physics_models(lookup)
				lookup["physics_model"] = physics_model_lookup
				self.export_physics_scene(lookup)

			self.export_animations(lookup)
	
			self.writel(S_GEOM, 0, '</library_geometries>')
			
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
	
			try:
				f = open(self.path, "wb")
			except:
				return False
	
			f.write(bytes('<?xml version="1.0" encoding="utf-8"?>\n', "UTF-8"))
			f.write(bytes('<COLLADA xmlns="http://www.collada.org/2008/03/COLLADASchema" version="1.5.0">\n', "UTF-8"))
	
			s = []
			for x in self.sections.keys():
				s.append(x)
			s.sort()
			for x in s:
				for l in self.sections[x]:
					f.write(bytes(l + "\n", "UTF-8"))
	
			f.write(bytes('<scene>\n', "UTF-8"))
			f.write(bytes('\t<instance_visual_scene url="#' + self.scene_name + '"/>\n', "UTF-8"))
			if (self.config["use_physics"] & (bpy.context.scene.rigidbody_world != None)):
				f.write(bytes('\t<instance_physics_scene url="#' + self.scene_name + '-physics' + '"/>\n', "UTF-8"))
			f.write(bytes('</scene>\n', "UTF-8"))
			f.write(bytes('</COLLADA>\n', "UTF-8"))
			
		finally:
			self.remove_export_meshes()
		
		return True

	def __init__(self, path, kwargs, operator):
		self.operator = operator
		self.scene = bpy.context.scene
		self.last_id = 0
		self.scene_name = os.path.basename(bpy.data.filepath)
		self.scene_name = self.scene_name.replace(".blend", "")
		self.sections = {}
		self.path = path
		self.skeleton_info = {}
		self.config = kwargs
		self.valid_nodes = []
		self.material_link_symbols = {}
		self.meshes_to_clear = []
		self.node_names = {}
		self.transform_matrix_scale = self.config["transform_type"] == 'MATRIX_SCALE'

def save(operator, context,
	filepath="",
	use_selection=False,
	**kwargs
	):

	exp = DaeExporter(filepath, kwargs, operator)
	exp.export()
	return {'FINISHED'}  # so the script wont run after we have batch exported.


