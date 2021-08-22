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

    def ref_id(self, id):
        if "#" != id[0]:
            return "#" + id
        else:
            return id

    def export_image(self, image, image_id):

        imgpath = "//" + image_id + ".png"
        save_path = os.path.abspath(os.path.join(
            os.path.dirname(self.path),
            IMAGE_PATH,
            os.path.basename(imgpath)))

        if (save_path not in self.image_cache):
            basedir = os.path.dirname(save_path)
            if (not os.path.isdir(basedir)):
                os.makedirs(basedir)

            image.save_render(save_path)
            self.image_cache |= {save_path}

        self.writel(S_IMGS, 1, '<image id="' + image_id +
                    '" name="' + image.name + '">')

        # the file path should be surrounded by <ref> tags

        xml_path = os.path.join(
            "./", os.path.relpath(save_path, os.path.dirname(self.path))).replace("\\", "/")
        self.writel(S_IMGS, 2, '<init_from><ref>' +
                    xml_path + '</ref></init_from>')
        self.writel(S_IMGS, 1, '</image>')

    def export_sampler2d(self, il, imgid, sampler_sids):
        if (imgid):
            sampler_sid = imgid + "-sampler"
            if sampler_sid in sampler_sids:
                return sampler_sid
            sampler_sids.add(sampler_sid)
            self.writel(S_FX, il, '<newparam sid="' + sampler_sid + '">')
            self.writel(S_FX, il+1, '<sampler2D>')
            self.writel(
                S_FX, il+2, '<instance_image url="{}"/>'.format(self.ref_id(imgid)))

            self.writel(S_FX, il+1, '</sampler2D>')
            self.writel(S_FX, il, '</newparam>')

            return sampler_sid
        else:
            return None

    def export_effect(self, material, effect_id, lookup):
        if material.use_nodes:
            self.export_nodes_effect(material, effect_id, lookup)
        else:
            self.export_generic_effect(material, effect_id, lookup)

    def export_generic_effect(self, material, effect_id):
        self.writel(S_FX, 1, '<effect id="' + effect_id +
                    '" name="' + material.name + '">')
        self.writel(S_FX, 2, '<profile_COMMON>')
        self.writel(S_FX, 3, '<technique sid="common">')
        shtype = "blinn"
        self.writel(S_FX, 4, '<' + shtype + '>')

        # Base Color

        self.writel(S_FX, 5, '<diffuse>')
        self.writel(S_FX, 6, '<color>' +
                    numarr_alpha(material.diffuse_color) + ' </color>')
        self.writel(S_FX, 5, '</diffuse>')

        self.writel(S_FX, 4, '</' + shtype + '>')

        self.writel(S_FX, 3, '</technique>')
        self.writel(S_FX, 2, '</profile_COMMON>')
        self.writel(S_FX, 1, '</effect>')

    def export_nodes_effect(self, material, effect_id, lookup):

        try:
            output_node = material.node_tree.get_output_node('ALL')
            surface_input = next(
                (input for input in output_node.inputs if input.identifier == 'Surface'),
                None)
            surface_node = surface_input.links[0].from_node
        except:
            self.export_generic_effect(material, effect_id)
            return

        self.writel(S_FX, 1, '<effect id="' + effect_id +
                    '" name="' + material.name + '">')

        shader_funcs = {
            bpy.types.ShaderNodeBsdfPrincipled: self.export_ShaderNodeBsdfPrincipled,
        }

        if type(surface_node) in shader_funcs.keys():
            shader_funcs[type(surface_node)](surface_node, lookup)
        else:
            self.export_unknown_node_shader(surface_node, lookup)

        self.writel(S_FX, 1, '</effect>')

    def export_unknown_node_shader(self, shader, lookup):
        try:
            color = self.tex_or_color(shader.inputs['Color'], lookup)
            sampler_sids = set()
            color_sampler = self.export_sampler2d(
                3, color['image_id'], sampler_sids)
        except:
            return

        self.writel(S_FX, 2, '<profile_COMMON>')
        self.writel(S_FX, 3, '<technique sid="common">')
        self.writel(S_FX, 4, '<lambert>')

        self.writel(S_FX, 5, '<diffuse>')
        self.writel(S_FX, 6, self.xml_tex_or_color(color, color_sampler))
        self.writel(S_FX, 5, '</diffuse>')

        self.writel(S_FX, 4, '</lambert>')
        self.writel(S_FX, 3, '</technique>')
        self.writel(S_FX, 2, '</profile_COMMON>')

    def export_ShaderNodeBsdfPrincipled(self, shader, lookup):
        self.writel(S_FX, 2, '<profile_COMMON>')

        base = self.tex_or_color(shader.inputs['Base Color'], lookup)
        emission = self.tex_or_color(shader.inputs['Emission'], lookup)
        alpha = self.tex_or_factor(shader.inputs['Alpha'], lookup)
        specular = self.tex_or_factor(shader.inputs['Specular'], lookup)
        normal = self.get_socket_image(shader.inputs['Normal'], lookup)

        sampler_sids = set()
        base_sampler = self.export_sampler2d(3, base['image_id'], sampler_sids)
        emission_sampler = self.export_sampler2d(
            3, emission['image_id'], sampler_sids)
        alpha_sampler = self.export_sampler2d(
            3, alpha['image_id'], sampler_sids)
        specular_sampler = self.export_sampler2d(
            3, specular['image_id'], sampler_sids)
        normal_sampler = self.export_sampler2d(3, normal, sampler_sids)

        self.writel(S_FX, 3, '<technique sid="common">')
        self.writel(S_FX, 4, '<blinn>')

        self.writel(S_FX, 5, '<diffuse>')
        self.writel(S_FX, 6, self.xml_tex_or_color(base, base_sampler))
        self.writel(S_FX, 5, '</diffuse>')

        self.writel(S_FX, 5, '<emission>')
        self.writel(S_FX, 6, self.xml_tex_or_color(emission, emission_sampler))
        self.writel(S_FX, 5, '</emission>')

        self.writel(S_FX, 5, '<transparent opaque="RGB_ONE">')
        self.writel(S_FX, 6, self.xml_tex_or_factor(alpha, alpha_sampler))
        self.writel(S_FX, 5, '</transparent>')

        self.writel(S_FX, 5, '<specular>')
        self.writel(S_FX, 6, self.xml_tex_or_factor(
            specular, specular_sampler))
        self.writel(S_FX, 5, '</specular>')

        self.writel(S_FX, 5, '<shininess>')
        self.writel(S_FX, 6, '<float>' +
                    strflt(shader.inputs['Roughness'].default_value) + '</float>')
        self.writel(S_FX, 5, '</shininess>')

        self.writel(S_FX, 5, '<index_of_refraction>')
        self.writel(S_FX, 6, '<float>' +
                    strflt(shader.inputs['IOR'].default_value) + '</float>')
        self.writel(S_FX, 5, '</index_of_refraction>')

        self.writel(S_FX, 4, '</blinn>')

        if normal_sampler:
            self.writel(S_FX, 4, '<extra>')
            self.writel(S_FX, 5, '<technique profile="FCOLLADA">')
            self.writel(S_FX, 6, '<bump bumptype="NORMALMAP">')
            self.writel(S_FX, 7, self.xml_tex(normal_sampler))
            self.writel(S_FX, 6, '</bump>')
            self.writel(S_FX, 5, '</technique>')
            self.writel(S_FX, 4, '</extra>')

        self.writel(S_FX, 3, '</technique>')
        self.writel(S_FX, 2, '</profile_COMMON>')

    # socket:NodeSocketColor()
    def tex_or_color(self, socket, lookup):
        result = {
            'image_id': self.get_socket_image(socket, lookup),
            'color': strarr(socket.default_value)
        }
        return result

    def tex_or_factor(self, socket, lookup):
        result = {
            'image_id': self.get_socket_image(socket, lookup),
            'factor': socket.default_value
        }
        return result

    def get_socket_image(self, socket, lookup):
        if socket.is_linked:
            linked_node = socket.links[0].from_node
            if type(linked_node) == bpy.types.ShaderNodeTexImage:
                image = linked_node.image
                image_id = lookup['image'].get(image, None)
                if not image_id:
                    image_id = self.gen_unique_id(image.name)
                    self.export_image(image, image_id)
                    lookup['image'][image] = image_id
                return image_id
        return None

    def xml_tex(self, sampler):
        return '<texture texture="'+sampler+'" texcoord="UVMap"/>'

    def xml_tex_or_color(self, tex_or_color, sampler):
        if sampler:
            return self.xml_tex(sampler)
        else:
            return '<color>' + tex_or_color['color'] + ' </color>'

    def xml_tex_or_factor(self, tex_or_factor, sampler):
        if sampler:
            return '<texture texture="'+sampler+'" texcoord="UVMap"/>'
        else:
            factor = tex_or_factor['factor']
            return '<color>' + strarr([factor, factor, factor, factor]) + '</color>'

    def get_mesh(self, depsgraph, node):

        # get a mesh for this node
        mesh = node.evaluated_get(depsgraph).to_mesh(
            preserve_all_data_layers=True, depsgraph=depsgraph)

        if not mesh or not len(mesh.polygons):
            # mesh has no polygons so abort
            return None

        # force triangulation if the mesh has polygons with more than 4 sides
        # corrupts custom normals
        force_triangluation = False
        for polygon in mesh.polygons:
            if (polygon.loop_total > 4):
                force_triangluation = True
                break

        if (force_triangluation):
            bm = bmesh.new()
            bm.from_mesh(mesh)
            bmesh.ops.triangulate(bm, faces=bm.faces)
            bm.to_mesh(mesh)
            bm.free()
            mesh.update(calc_edges=True, calc_edges_loose=False)

        if not mesh.has_custom_normals and mesh.use_auto_smooth:
            mesh.calc_normals_split()

        if self.use_tangents:
            mesh.calc_tangents()

        return mesh

    def average_color(self, color, count):
        if count != 0:
            return Vector(color / count).freeze()
        else:
            return Vector(color).freeze()

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
            color_buckets = [[0, Vector((0, 0, 0, 0))]
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
        # colors = array of r,g,b,a color tuples
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
        if (node.rigid_body and
            node.rigid_body.collision_shape == 'CONVEX_HULL' and
                node.rigid_body.mesh_source == 'BASE'):
            return True

        return False

    def mesh_to_convex_hull(self, mesh):
        # use blender to generate a convex hull for the mesh

        # just need the vertices for hull generation

        bm = bmesh.new()
        for vert in mesh.vertices.values():
            bm.verts.new(vert.co)
        bm.verts.ensure_lookup_table()

        # Generate hull
        ret = bmesh.ops.convex_hull(
            bm, input=bm.verts, use_existing_faces=False)

        # Delete the vertices that weren't used in the convex hull

        geom = ret["geom_unused"]
        # there is no documentation for values of 'context'. 1 works to
        # delete vertices
        bmesh.ops.delete(bm, geom=geom, context=1)

        # convert bmesh back to mesh
        me = bpy.data.meshes.new(self.new_id("DAE_convex_hull"))
        bm.to_mesh(me)
        return me

    def export_meshes(self, depsgraph, lookup):

        mesh_nodes = {node for node in self.visual_nodes
                      if (node.type == "MESH" or node.type == "CURVE")}

        for node in mesh_nodes:
            has_modifiers = self.node_has_generate_modifiers(node)
            if (not has_modifiers and node.data in lookup["data_to_mesh"]):
                # A linked duplicate with no modifiers to make it any different from the original
                lookup["node_to_mesh"][node] = lookup["data_to_mesh"][node.data]
                continue

            # generate mesh from node

            mesh = self.get_mesh(depsgraph, node)

            # export the mesh
            if (mesh):
                if not self.node_has_morphs(node):
                    mesh_id = self.gen_unique_id(node.data.name)
                    material_bind = self.export_mesh(
                        mesh, mesh_id, node.data.name)

                    # export convex hull if needed by physics scene

                    if (self.node_has_convex_hull(node)):
                        convex_mesh = self.mesh_to_convex_hull(mesh)
                        convex_mesh_id = mesh_id + "-convex"
                        self.export_mesh(
                            convex_mesh, convex_mesh_id, node.data.name, True)
                        lookup["convex_mesh"][node] = convex_mesh_id

                    morphs = None
                else:
                    mesh_id = None
                    material_bind = None
                    # export morphs from shape keys

                    morphs = self.export_mesh_morphs(node)
                    if morphs:
                        material_bind = morphs[0]['material_bind']

                mesh_lookup = {"id": mesh_id,
                               "material_bind": material_bind, "morphs": morphs}
                lookup["node_to_mesh"][node] = mesh_lookup

                # Modifiers will generate a different mesh even if it uses the same mesh data
                # so dont' rememer this generated mesh for later instancing
                if not has_modifiers:
                    lookup["data_to_mesh"][node.data] = mesh_lookup

            else:
                if(node.type == "CURVE"):
                    # All else failed so export a Bezier curve
                    curve_id = self.gen_unique_id(node.data.name)
                    self.export_curve(node.data, curve_id)
                    curve_lookup = {"id": curve_id,
                                    "material_bind": None, "morphs": None}
                    lookup["node_to_mesh"][node] = curve_lookup

    def export_mesh_morphs(self, node):
        if self.node_has_morphs(node):
            mesh = node.data
            values = []

            # save weight values for restoration after being monkeyed with

            for k in range(0, len(mesh.shape_keys.key_blocks)):
                shape = node.data.shape_keys.key_blocks[k]
                values += [shape.value]
                shape.value = 0

            scene_show_only_shape_key = node.show_only_shape_key
            scene_active_shape_key = node.active_shape_key_index

            morph_targets = []

            # pose the mesh using each shape key and export a mesh for each

            for k in range(0, len(mesh.shape_keys.key_blocks)):
                shape = node.data.shape_keys.key_blocks[k]
                node.show_only_shape_key = True
                node.active_shape_key_index = k
                shape.value = 1.0
                mesh.update()
                morph_id = self.gen_unique_id(
                    node.data.name + "-" + shape.name)
                export_mesh = self.get_mesh(
                    bpy.context.evaluated_depsgraph_get(), node)
                material_bind = self.export_mesh(
                    export_mesh, morph_id, morph_id)
                morph_targets.append(
                    {"id": morph_id, "material_bind": material_bind, "weight": values[k]})
                shape.value = values[k]

            node.show_only_shape_key = scene_show_only_shape_key
            node.active_shape_key_index = scene_active_shape_key
            mesh.update()

            # morph_targets = [id of morph shape mesh, material bind, the shape key weight]
            # ([0] is the basis)
            return morph_targets
        else:
            return None

    def export_morph_controllers(self, depsgraph, lookup):
        geometry_morphs = {
            node: value for node, value in lookup["node_to_mesh"].items()
            if value["morphs"] != None}

        # Create a key:controller id dict based on shape keys to perform deduplication.
        # Morph controllers that have the same shape keys for the same meshes must
        # actually be the same morph controller

        targets_to_morph_controller = {}
        for node, values in geometry_morphs.items():
            if node not in lookup["node_to_morph_controller"]:
                targets = values["morphs"]
                key_targets = ".".join(t['id'] for t in targets)
                if not key_targets in targets_to_morph_controller:
                    morph_id = self.gen_unique_id(node.data.name + "-morph")
                    self.export_morph_controller(targets, morph_id, True)
                    lookup["node_to_morph_controller"][node] = morph_id
                    targets_to_morph_controller[key_targets] = morph_id
                else:
                    lookup["node_to_morph_controller"][node] = targets_to_morph_controller[key_targets]

    def export_morph_controller(self, morph_targets, morph_id, relative):
        self.writel(S_MORPH, 1, '<controller id="' +
                    morph_id + '" name="' + morph_id + '">')
        if relative:
            method = "RELATIVE"
        else:
            method = "NORMALIZED"
        self.writel(S_MORPH, 2, '<morph source="' +
                    self.ref_id(morph_targets[0]["id"]) + '" method="'+method+'">')
        source_targets_id = self.gen_unique_id(morph_id + '-targets')
        self.writel(S_MORPH, 3, '<source id="' + source_targets_id + '">')
        targets_array_id = self.gen_unique_id(morph_id + '-targets-array')
        self.writel(S_MORPH, 4, '<IDREF_array id="' + targets_array_id +
                    '" count="' + str(len(morph_targets)-1) + '">')
        marr = " ".join(target["id"] for target in morph_targets[1:])
        warr = " ".join(strflt(target["weight"])
                        for target in morph_targets[1:])
        self.writel(S_MORPH, 5, marr)
        self.writel(S_MORPH, 4, '</IDREF_array>')
        self.writel(S_MORPH, 4, '<technique_common>')
        self.writel(S_MORPH, 5, '<accessor source="' + self.ref_id(targets_array_id)+'" count="'
                    + str(len(morph_targets)-1) + '" stride="1">')
        self.writel(S_MORPH, 6, '<param name="MORPH_TARGET" type="IDREF"/>')
        self.writel(S_MORPH, 5, '</accessor>')
        self.writel(S_MORPH, 4, '</technique_common>')
        self.writel(S_MORPH, 3, '</source>')
        source_weights_id = self.gen_unique_id(morph_id + '-weights')
        self.writel(S_MORPH, 3, '<source id="' + source_weights_id + '">')
        array_weights_id = self.gen_unique_id(morph_id + '-weights-array')
        self.writel(S_MORPH, 4, '<float_array id="' + array_weights_id +
                    '" count="' + str(len(morph_targets)-1) + '" >')
        self.writel(S_MORPH, 5, warr)
        self.writel(S_MORPH, 4, '</float_array>')
        self.writel(S_MORPH, 4, '<technique_common>')
        self.writel(S_MORPH, 5, '<accessor source="' + self.ref_id(array_weights_id) +
                    '" count="' + str(len(morph_targets)-1) + '" stride="1">')
        self.writel(S_MORPH, 6, '<param name="MORPH_WEIGHT" type="float"/>')
        self.writel(S_MORPH, 5, '</accessor>')
        self.writel(S_MORPH, 4, '</technique_common>')
        self.writel(S_MORPH, 3, '</source>')
        self.writel(S_MORPH, 3, '<targets>')
        self.writel(
            S_MORPH, 4, '<input source="' + self.ref_id(source_targets_id)+'" semantic="MORPH_TARGET" />')
        self.writel(
            S_MORPH, 4, '<input source="' + self.ref_id(source_weights_id)+'" semantic="MORPH_WEIGHT"/>')
        self.writel(S_MORPH, 3, '</targets>')

        # Extension for allowing morph controller animation
        # https://www.khronos.org/collada/wiki/Morph_weights_KHR_extension

        self.writel(S_MORPH, 3, '<extra>')
        self.writel(S_MORPH, 4, '<technique profile="KHR">')
        self.writel(S_MORPH, 5, '<morph_weights sid="MORPH_WEIGHT_TO_TARGET" count="' +
                    str(len(morph_targets)-1)+'">')
        self.writel(S_MORPH, 6, warr)
        self.writel(S_MORPH, 5, '</morph_weights>')
        self.writel(S_MORPH, 4, '</technique>')
        self.writel(S_MORPH, 3, '</extra>')

        self.writel(S_MORPH, 2, '</morph>')
        self.writel(S_MORPH, 1, '</controller>')

    def node_has_generate_modifiers(self, node):
        if hasattr(node, "modifiers"):
            has = next(
                (mod for mod in node.modifiers if mod.show_render and mod.type != "ARMATURE"), None) != None
        else:
            has = False

        return has

    def node_skin_modifiers(self, node):
        if hasattr(node, "modifiers"):
            return (mod for mod in node.modifiers if mod.show_render and mod.type == "ARMATURE" and mod.use_vertex_groups)
        else:
            return ()

    def node_has_skin_modifier(self, node):
        return next((self.node_skin_modifiers(node)), None) != None

    def export_skin_controllers(self, depsgraph, lookup):
        meshes = {node
                  for node in self.visual_nodes
                  if node not in lookup["node_to_skin"] and self.node_has_skin_modifier(node)}

        for node in meshes:
            armatures = self.node_skin_modifiers(node)
            for armature in armatures:
                skin_id = self.gen_unique_id(
                    node.name + "-" + armature.object.name + "-skin")
                lu = {"skin": skin_id, "skeleton": armature.object.name}
                if (not node in lookup["node_to_skin"]):
                    lookup["node_to_skin"][node] = []
                lookup["node_to_skin"][node].append(lu)
                mesh_id = lookup["node_to_mesh"][node]["id"]
                morph_id = lookup["node_to_morph_controller"].get(node, None)
                if morph_id:
                    attached_id = morph_id
                else:
                    attached_id = mesh_id
                self.export_skin_controller(
                    depsgraph, node, armature.object, attached_id, skin_id)

    def export_skin_controller(self, depsgraph, node, armature, mesh_id, skin_id):

        if not self.overstuff_bones:
            group_names = [group.name for group in node.vertex_groups.values(
            ) if group.name in armature.data.bones]
        else:
            # put every bone from the armature into the skin because reasons

            group_names = [group for group in armature.data.bones.keys()]

        missing_group_names = {group.name for group in node.vertex_groups.values(
        ) if group.name not in armature.data.bones}
        group_names_index = dict({k: v for (v, k) in enumerate(group_names)}.items() | {
            k: -1 for k in missing_group_names}.items())

        mesh = self.get_mesh(depsgraph, node)

        self.writel(S_SKIN, 1, '<controller id="' + skin_id + '">')
        self.writel(S_SKIN, 2, '<skin source="' + self.ref_id(mesh_id) + '">')
        self.writel(S_SKIN, 3, '<bind_shape_matrix>' +
                    self.strmtx(node.matrix_world) + '</bind_shape_matrix>')

        # Joint Names

        source_joints_id = self.gen_unique_id(skin_id + '-joints')
        self.writel(S_SKIN, 3, '<source id="'+source_joints_id + '">')
        name_values = " ".join([self.quote_spaces(name)
                                for name in group_names])
        name_array_joints_id = self.gen_unique_id(skin_id + '-joints-array')
        self.writel(S_SKIN, 4, '<Name_array id="'+name_array_joints_id + '" count="' +
                    str(len(group_names)) + '">' + name_values + '</Name_array>')
        self.writel(S_SKIN, 4, '<technique_common>')
        self.writel(S_SKIN, 4, '<accessor source="'+self.ref_id(name_array_joints_id) +
                    '" count="' + str(len(group_names)) + '" stride="1">')
        self.writel(S_SKIN, 5, '<param name="JOINT" type="Name"/>')
        self.writel(S_SKIN, 4, '</accessor>')
        self.writel(S_SKIN, 4, '</technique_common>')
        self.writel(S_SKIN, 3, '</source>')

        # Pose Matrices!

        bones = [armature.data.bones[name] for name in group_names]
        pose_matrices = [bone.matrix_local.inverted() for bone in bones]

        source_bind_poses_id = self.gen_unique_id(skin_id + '-bind_poses')
        self.writel(S_SKIN, 3, '<source id="' + source_bind_poses_id + '">')
        pose_values = " ".join([self.strmtx(matrix)
                                for matrix in pose_matrices])
        array_bind_poses_id = self.gen_unique_id(skin_id + '-bind_poses-array')
        self.writel(S_SKIN, 4, '<float_array id="' + array_bind_poses_id+'" count="' +
                    str(len(pose_matrices) * 16) + '">' + pose_values + '</float_array>')
        self.writel(S_SKIN, 4, '<technique_common>')
        self.writel(S_SKIN, 4, '<accessor source="' + self.ref_id(array_bind_poses_id) +
                    '" count="' + str(len(pose_matrices)) + '" stride="16">')
        self.writel(S_SKIN, 5, '<param name="TRANSFORM" type="float4x4"/>')
        self.writel(S_SKIN, 4, '</accessor>')
        self.writel(S_SKIN, 4, '</technique_common>')
        self.writel(S_SKIN, 3, '</source>')

        # Skin Weights!

        weights = list(
            {group.weight for v in mesh.vertices for group in v.groups})
        weights_index = {k: v for (v, k) in enumerate(weights)}
        vertex_weights = [[[group_names_index[node.vertex_groups[g.group].name],
                            weights_index[g.weight]] for g in v.groups] for v in mesh.vertices]
        vertex_weights = [[g for g in v if g[0] != -1] for v in vertex_weights]
        weight_counts = [len(v) for v in vertex_weights]

        source_skin_weights_id = self.gen_unique_id(skin_id + '-weights')
        self.writel(S_SKIN, 3, '<source id="'+source_skin_weights_id + '">')
        array_weights_id = self.gen_unique_id(skin_id + '-weights-array')
        self.writel(S_SKIN, 4, '<float_array id="' + array_weights_id+'" count="' +
                    str(len(weights)) + '">' + " ".join([strflt(w) for w in weights]) + '</float_array>')
        self.writel(S_SKIN, 4, '<technique_common>')
        self.writel(S_SKIN, 4, '<accessor source="' + self.ref_id(array_weights_id) +
                    '" count="' + str(len(weights)) + '" stride="1">')
        self.writel(S_SKIN, 5, '<param name="WEIGHT" type="float"/>')
        self.writel(S_SKIN, 4, '</accessor>')
        self.writel(S_SKIN, 4, '</technique_common>')
        self.writel(S_SKIN, 3, '</source>')
        self.writel(S_SKIN, 3, '<joints>')
        self.writel(S_SKIN, 4, '<input semantic="JOINT" source="' +
                    self.ref_id(source_joints_id)+'"/>')
        self.writel(S_SKIN, 4, '<input semantic="INV_BIND_MATRIX" source="'
                    + self.ref_id(source_bind_poses_id)+'"/>')
        self.writel(S_SKIN, 3, '</joints>')
        self.writel(S_SKIN, 3, '<vertex_weights count="' +
                    str(len(weight_counts)) + '">')
        self.writel(S_SKIN, 4, '<input semantic="JOINT" source="' +
                    self.ref_id(source_joints_id)+'" offset="0"/>')
        self.writel(S_SKIN, 4, '<input semantic="WEIGHT" source="' +
                    self.ref_id(source_skin_weights_id) + '" offset="1"/>')
        self.writel(S_SKIN, 4, '<vcount>' +
                    " ".join([str(c) for c in weight_counts]) + '</vcount>')
        self.writel(S_SKIN, 4, '<v>' +
                    " ".join([str(i) for v in vertex_weights for g in v for i in g]) + '</v>')
        self.writel(S_SKIN, 3, '</vertex_weights>')
        self.writel(S_SKIN, 2, '</skin>')
        self.writel(S_SKIN, 1, '</controller>')

    def node_has_morphs(self, node):
        return node.data and node.data.shape_keys and len(node.data.shape_keys.key_blocks)

    def export_mesh(self, mesh, mesh_id, mesh_name, convex=False):

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
        has_split_normals = len(split_normals) > 0
        if has_split_normals:
            normals = split_normals
            has_normals = True
        else:
            has_normals = len(normals) > 0
        has_uv = (len(uv) > 0)
        has_colors = (has_vertex and len(colors) > 0)
        has_tangents = (len(tangents) > 0)
        has_bitangents = (
            has_normals and has_tangents and (len(bitangents) > 0))

        self.writel(S_GEOM, 1, '<geometry id="' +
                    mesh_id + '" name="' + mesh_name + '">')
        if (convex):
            self.writel(S_GEOM, 2, '<convex_mesh>')
        else:
            self.writel(S_GEOM, 2, '<mesh>')

        # Vertex Array
        if has_vertex:
            source_positions_id = self.gen_unique_id(mesh_id + '-positions')
            self.writel(S_GEOM, 3, '<source id="' + source_positions_id+'">')
            float_values = " ".join([self.strxyz(v) for v in vertices])
            array_positions_id = self.gen_unique_id(
                mesh_id + '-positions-array')
            self.writel(S_GEOM, 4, '<float_array id="' + array_positions_id+'" count="' +
                        str(len(vertices) * 3) + '">' + float_values + '</float_array>')
            self.writel(S_GEOM, 4, '<technique_common>')
            self.writel(S_GEOM, 5, '<accessor source="' + self.ref_id(array_positions_id) +
                        '" count="' + str(len(vertices)) + '" stride="3">')
            self.writel(S_GEOM, 6, '<param name="X" type="float"/>')
            self.writel(S_GEOM, 6, '<param name="Y" type="float"/>')
            self.writel(S_GEOM, 6, '<param name="Z" type="float"/>')
            self.writel(S_GEOM, 5, '</accessor>')
            self.writel(S_GEOM, 4, '</technique_common>')
            self.writel(S_GEOM, 3, '</source>')

        # Normal Array
        if has_normals:
            source_normals_id = self.gen_unique_id(mesh_id + '-normals')
            self.writel(S_GEOM, 3, '<source id="' + source_normals_id + '">')
            float_values = " ".join([self.strxyz(v) for v in normals])
            array_normals_id = self.gen_unique_id(mesh_id + '-normals-array')
            self.writel(S_GEOM, 4, '<float_array id="' + array_normals_id + '" count="' +
                        str(len(normals) * 3) + '">' + float_values + '</float_array>')
            self.writel(S_GEOM, 4, '<technique_common>')
            self.writel(S_GEOM, 5, '<accessor source="' + self.ref_id(array_normals_id) +
                        '" count="' + str(len(normals)) + '" stride="3">')
            self.writel(S_GEOM, 6, '<param name="X" type="float"/>')
            self.writel(S_GEOM, 6, '<param name="Y" type="float"/>')
            self.writel(S_GEOM, 6, '<param name="Z" type="float"/>')
            self.writel(S_GEOM, 5, '</accessor>')
            self.writel(S_GEOM, 4, '</technique_common>')
            self.writel(S_GEOM, 3, '</source>')

        # UV Arrays
        if has_uv:
            source_texcoord_id = self.gen_unique_id(mesh_id + '-texcoord')
            self.writel(S_GEOM, 3, '<source id="'+source_texcoord_id + '">')
            float_values = " ".join(
                [strflt(c) for v in [[v.x, v.y] for v in uv] for c in v])
            array_texcoord_id = self.gen_unique_id(mesh_id + '-texcoord-array')
            self.writel(S_GEOM, 4, '<float_array id="' + array_texcoord_id+'" count="' +
                        str(len(uv) * 2) + '">' + float_values + '</float_array>')
            self.writel(S_GEOM, 4, '<technique_common>')
            self.writel(S_GEOM, 5, '<accessor source="' + self.ref_id(array_texcoord_id) +
                        '" count="' + str(len(uv)) + '" stride="2">')
            self.writel(S_GEOM, 6, '<param name="S" type="float"/>')
            self.writel(S_GEOM, 6, '<param name="T" type="float"/>')
            self.writel(S_GEOM, 5, '</accessor>')
            self.writel(S_GEOM, 4, '</technique_common>')
            self.writel(S_GEOM, 3, '</source>')

        # Color Arrays

        if has_colors:
            source_colors_id = self.gen_unique_id(mesh_id + '-colors')
            self.writel(S_GEOM, 3, '<source id="' + source_colors_id+'">')
            float_values = " ".join(
                [strflt(c) for v in [[v[0], v[1], v[2], v[3]] for v in colors] for c in v])
            array_colors_id = self.gen_unique_id(mesh_id + '-colors-array')
            self.writel(S_GEOM, 4, '<float_array id="' + array_colors_id + '" count="' +
                        str(len(colors) * 4) + '">' + float_values + '</float_array>')
            self.writel(S_GEOM, 4, '<technique_common>')
            self.writel(S_GEOM, 5, '<accessor source="' + self.ref_id(array_colors_id) +
                        '" count="' + str(len(colors)) + '" stride="4">')
            self.writel(S_GEOM, 6, '<param name="R" type="float"/>')
            self.writel(S_GEOM, 6, '<param name="G" type="float"/>')
            self.writel(S_GEOM, 6, '<param name="B" type="float"/>')
            self.writel(S_GEOM, 6, '<param name="A" type="float"/>')
            self.writel(S_GEOM, 5, '</accessor>')
            self.writel(S_GEOM, 4, '</technique_common>')
            self.writel(S_GEOM, 3, '</source>')

        if has_tangents:
            source_tangents_id = self.gen_unique_id(mesh_id+'-tangents')
            self.writel(S_GEOM, 3, "<source id=\"{}\">".format(
                source_tangents_id))
            float_values = " ".join([self.strxyz(v) for v in tangents])
            array_tangents_id = self.gen_unique_id(mesh_id+'-tangents-array')
            self.writel(S_GEOM, 4, "<float_array id=\"{}\" count=\"{}\">{}</float_array>"
                        .format(array_tangents_id, len(tangents) * 3, float_values))
            self.writel(S_GEOM, 4, "<technique_common>")
            self.writel(S_GEOM, 4, "<accessor source=\"{}\" count=\"{}\" stride=\"3\">"
                        .format(self.ref_id(array_tangents_id), len(tangents)))
            self.writel(S_GEOM, 5, "<param name=\"X\" type=\"float\"/>")
            self.writel(S_GEOM, 5, "<param name=\"Y\" type=\"float\"/>")
            self.writel(S_GEOM, 5, "<param name=\"Z\" type=\"float\"/>")
            self.writel(S_GEOM, 4, "</accessor>")
            self.writel(S_GEOM, 4, "</technique_common>")
            self.writel(S_GEOM, 3, "</source>")

        if has_bitangents:
            source_bitangents_id = self.gen_unique_id(mesh_id + '-bitangents')
            self.writel(S_GEOM, 3, "<source id=\"{}\">".format(
                source_bitangents_id))
            float_values = " ".join([self.strxyz(v) for v in bitangents])
            array_bitangents_id = self.gen_unique_id(
                mesh_id+'-bitangents-array')
            self.writel(S_GEOM, 4, "<float_array id=\"{}\"count=\"{}\">{}</float_array>"
                        .format(array_bitangents_id, len(bitangents) * 3, float_values))
            self.writel(S_GEOM, 4, "<technique_common>")
            self.writel(S_GEOM, 4, "<accessor source=\"{}\" count=\"{}\" stride=\"3\">"
                        .format(self.ref_id(array_bitangents_id), len(bitangents)))
            self.writel(S_GEOM, 5, "<param name=\"X\" type=\"float\"/>")
            self.writel(S_GEOM, 5, "<param name=\"Y\" type=\"float\"/>")
            self.writel(S_GEOM, 5, "<param name=\"Z\" type=\"float\"/>")
            self.writel(S_GEOM, 4, "</accessor>")
            self.writel(S_GEOM, 4, "</technique_common>")
            self.writel(S_GEOM, 3, "</source>")

        # Vertices
        vertices_id = self.gen_unique_id(mesh_id + '-vertices')
        self.writel(S_GEOM, 3, '<vertices id="' + vertices_id + '">')
        if (has_vertex):
            self.writel(S_GEOM, 4, '<input semantic="POSITION" source="' +
                        self.ref_id(source_positions_id) + '"/>')
        if (has_normals and not has_split_normals):
            self.writel(
                S_GEOM, 4, '<input semantic="NORMAL" source="' + self.ref_id(source_normals_id) + '"/>')
        self.writel(S_GEOM, 3, '</vertices>')

        material_bind = {}

        # calculate offsets and layout of <p> indices
        offset = 0
        if has_vertex:
            vertex_offset = offset
            offset += 1
        if has_split_normals:
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
        if has_colors:
            color_offset = offset
            offset += 1
        if has_uv:
            uv_offset = offset
            offset += 1

        stride = offset

        for mat_index, polygons in surface_v_indices.items():

            # Triangle Lists
            triangulated = not next(
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
                            self.ref_id(vertices_id) + '" offset="' + str(vertex_offset) + '"/>')
            if (has_normals):
                self.writel(S_GEOM, 4, '<input semantic="NORMAL" source="' +
                            self.ref_id(source_normals_id) + '" offset="' + str(normal_offset) + '"/>')
            if (has_tangents):
                self.writel(S_GEOM, 4, '<input semantic="TEXTANGENT" source="' +
                            self.ref_id(source_tangents_id) + '" offset="' + str(tangent_offset) + '" set="0"/>')
            if (has_bitangents):
                self.writel(S_GEOM, 4, '<input semantic="TEXBINORMAL" source="' +
                            self.ref_id(source_bitangents_id) + '" offset="' + str(bitangent_offset) + '" set="0"/>')
            if (has_colors):
                self.writel(S_GEOM, 4, '<input semantic="COLOR" source="' +
                            self.ref_id(source_colors_id) + '" offset="' + str(color_offset) + '"/>')
            if (has_uv):
                self.writel(S_GEOM, 4, '<input semantic="TEXCOORD" source="' +
                            self.ref_id(source_texcoord_id) + '" offset="' + str(uv_offset) + '" set="0"/>')

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

    def gen_unique_id(self, name):
        count = 0
        fix_name = name.replace(" ", "_")
        fix_name = fix_name.replace("/", "_")
        fix_name = fix_name.replace("\\", "_")
        node_id = fix_name
        while node_id in self.node_ids:
            count += 1
            node_id = fix_name+"-" + str(count)
        self.node_ids.add(node_id)
        return node_id

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
        bpy.context.view_layer.update()
        self.settle_ik()

    def restore_scene_pose(self):
        for node, pose_position in self.pose_state.items():
            node.data.pose_position = pose_position
        bpy.context.view_layer.update()

    def get_bone_deform_parent(self, bone):
        return bone.parent

    def get_bone_transform(self, bone):
        # get the transform relative to the parent bone.

        parent = self.get_bone_deform_parent(bone)
        if (parent != None) and not self.is_zero_scale(parent.matrix_local):
            matrix = parent.matrix_local.inverted() @ bone.matrix_local
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
                    matrix = parent.matrix.inverted() @ matrix
                else:
                    return None

        if (self.transform_matrix_scale):
            matrix, scale = self.split_matrix_scale(matrix)
            return {"matrix": matrix, "scale": scale}
        else:
            return {"matrix": matrix}

    def get_bone_transform_xml(self, bone):
        return self.transform_to_xml(self.get_bone_transform(bone))

    def export_armature_bone(self, section, il, bone, armature, parenting_map, recurse, lookup):

        boneid = lookup["nodes"][armature] + "/" + bone.name
        lookup["skeleton_info"][armature][bone.name] = boneid
        self.writel(section, il, '<node sid="' + bone.name +
                    '" name="'+bone.name+'" type="JOINT">')
        il += 1

        transforms = self.get_bone_transform_xml(bone)
        for t in transforms:
            self.writel(section, il, t)

        # export nodes that are parented to this bone

        node_children = parenting_map.get(bone.name, None)
        if (recurse and node_children):
            for c in node_children:
                self.export_node(section, c, il, recurse, lookup)

        for c in bone.children:
            self.export_armature_bone(
                section, il, c, armature, parenting_map, recurse, lookup)
        il -= 1
        self.writel(section, il, '</node>')

    def export_armature_node(self, section, il, node, recurse, lookup):
        parenting_map = self.get_armature_children_of_bones(node)

        armature = node.data

        lookup["skeleton_info"][node] = {}

        for bone in armature.bones:
            if (bone.parent != None):
                # this node will be exported when the parent exports its
                # children
                continue
            self.export_armature_bone(
                section, il, bone, node, parenting_map, recurse, lookup)

    def export_cameras(self, lookup):
        cameras = {node.data for node in self.visual_nodes if node.type ==
                   "CAMERA" and node.data not in lookup["camera"]}

        for camera in cameras:
            camera_id = self.gen_unique_id(camera.name + "-camera")
            lookup["camera"][camera] = camera_id
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
        lights = {node.data for node in self.visual_nodes if node.type ==
                  "LIGHT" and node.data not in lookup["light"]}
        for light in lights:
            light_id = self.gen_unique_id(light.name + "-light")
            lookup["light"][light] = light_id
            self.export_lamp(light, light_id)

    def export_lamp(self, light, light_id):

        self.writel(S_LAMPS, 1, '<light id="' + light_id +
                    '" name="' + light.name + '">')
        # self.writel(S_LAMPS,2,'<optics>')
        self.writel(S_LAMPS, 3, '<technique_common>')

        if (light.type == "POINT"):
            self.writel(S_LAMPS, 4, '<point>')
            self.writel(S_LAMPS, 5, '<color>' +
                        strarr(light.color) + '</color>')
            self.writel(S_LAMPS, 5, '<linear_attenuation>' +
                        strflt(light.linear_attenuation) + '</linear_attenuation>')
            self.writel(S_LAMPS, 5, '<quadratic_attenuation>' +
                        strflt(light.quadratic_attenuation) + '</quadratic_attenuation>')
            self.writel(S_LAMPS, 5, '<zfar>' +
                        strflt(light.cutoff_distance) + '</zfar>')
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

        # curve node points positions

        source_positions_id = self.gen_unique_id(spline_id + '-positions')
        self.writel(S_GEOM, 3, '<source id="' + source_positions_id + '">')
        position_values = " ".join([self.strxyz(x) for x in points])
        array_positions_id = self.gen_unique_id(spline_id + '-positions-array')
        self.writel(S_GEOM, 4, '<float_array id="'+array_positions_id + '" count="' +
                    str(len(points) * 3) + '">' + position_values + '</float_array>')
        self.writel(S_GEOM, 4, '<technique_common>')
        self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(array_positions_id) +
                    '" count="' + str(len(points)) + '" stride="3">')
        self.writel(S_GEOM, 5, '<param name="X" type="float"/>')
        self.writel(S_GEOM, 5, '<param name="Y" type="float"/>')
        self.writel(S_GEOM, 5, '<param name="Z" type="float"/>')
        self.writel(S_GEOM, 4, '</accessor>')
        self.writel(S_GEOM, 4, '</technique_common>')
        self.writel(S_GEOM, 3, '</source>')

        # in tangents

        source_intangents_id = self.gen_unique_id(spline_id + '-intangents')
        self.writel(S_GEOM, 3, '<source id="' + source_intangents_id+'">')
        intangent_values = " ".join([self.strxyz(x) for x in handles_in])
        array_intangents = self.gen_unique_id(spline_id + '-intangents-array')
        self.writel(S_GEOM, 4, '<float_array id="' + array_intangents+'" count="' +
                    str(len(points) * 3) + '">' + intangent_values + '</float_array>')
        self.writel(S_GEOM, 4, '<technique_common>')
        self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(array_intangents) +
                    '" count="' + str(len(points)) + '" stride="3">')
        self.writel(S_GEOM, 5, '<param name="X" type="float"/>')
        self.writel(S_GEOM, 5, '<param name="Y" type="float"/>')
        self.writel(S_GEOM, 5, '<param name="Z" type="float"/>')
        self.writel(S_GEOM, 4, '</accessor>')
        self.writel(S_GEOM, 4, '</technique_common>')
        self.writel(S_GEOM, 3, '</source>')

        # out tangents

        source_outtangents_id = self.gen_unique_id(spline_id + '-outtangents')
        self.writel(S_GEOM, 3, '<source id="' + source_outtangents_id + '">')
        outtangent_values = " ".join([self.strxyz(x) for x in handles_out])
        array_outtangents_id = self.gen_unique_id(
            spline_id + '-outtangents-array')
        self.writel(S_GEOM, 4, '<float_array id="' + array_outtangents_id + '" count="' +
                    str(len(points) * 3) + '">' + outtangent_values + '</float_array>')
        self.writel(S_GEOM, 4, '<technique_common>')
        self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(array_outtangents_id) +
                    '" count="' + str(len(points)) + '" stride="3">')
        self.writel(S_GEOM, 5, '<param name="X" type="float"/>')
        self.writel(S_GEOM, 5, '<param name="Y" type="float"/>')
        self.writel(S_GEOM, 5, '<param name="Z" type="float"/>')
        self.writel(S_GEOM, 4, '</accessor>')
        self.writel(S_GEOM, 4, '</technique_common>')
        self.writel(S_GEOM, 3, '</source>')

        # interpolations

        source_interpolations_id = self.gen_unique_id(
            spline_id + '-interpolations')
        self.writel(S_GEOM, 3, '<source id="' +
                    source_interpolations_id + '">')
        interpolation_values = " ".join([str(x) for x in interps])
        array_interpolations_id = self.gen_unique_id(
            spline_id + '-interpolations-array')
        self.writel(S_GEOM, 4, '<Name_array id="'+array_interpolations_id + '" count="' +
                    str(len(interps)) + '">' + interpolation_values + '</Name_array>')
        self.writel(S_GEOM, 4, '<technique_common>')
        self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(array_interpolations_id) +
                    '" count="' + str(len(interps)) + '" stride="1">')
        self.writel(S_GEOM, 5, '<param name="INTERPOLATION" type="name"/>')
        self.writel(S_GEOM, 4, '</accessor>')
        self.writel(S_GEOM, 4, '</technique_common>')
        self.writel(S_GEOM, 3, '</source>')

        # tilts

        source_tilts_id = self.gen_unique_id(spline_id + '-tilts')
        self.writel(S_GEOM, 3, '<source id="'+source_tilts_id+'">')
        tilt_values = " ".join([strflt(x) for x in tilts])
        array_tilts_id = self.gen_unique_id(spline_id + '-tilts-array')
        self.writel(S_GEOM, 4, '<float_array id="' + array_tilts_id + '" count="' +
                    str(len(tilts)) + '">' + tilt_values + '</float_array>')
        self.writel(S_GEOM, 4, '<technique_common>')
        self.writel(S_GEOM, 4, '<accessor source="' + self.ref_id(array_tilts_id) +
                    '" count="' + str(len(tilts)) + '" stride="1">')
        self.writel(S_GEOM, 5, '<param name="TILT" type="float"/>')
        self.writel(S_GEOM, 4, '</accessor>')
        self.writel(S_GEOM, 4, '</technique_common>')
        self.writel(S_GEOM, 3, '</source>')

        # vertices

        self.writel(S_GEOM, 3, '<control_vertices>')
        self.writel(S_GEOM, 4, '<input semantic="POSITION" source="' +
                    self.ref_id(source_positions_id)+'"/>')
        self.writel(S_GEOM, 4, '<input semantic="IN_TANGENT" source="' +
                    self.ref_id(source_intangents_id) + '"/>')
        self.writel(S_GEOM, 4, '<input semantic="OUT_TANGENT" source="' +
                    self.ref_id(source_outtangents_id) + '"/>')
        self.writel(S_GEOM, 4, '<input semantic="INTERPOLATION" source="' +
                    self.ref_id(source_interpolations_id) + '"/>')
        self.writel(S_GEOM, 4, '<input semantic="TILT" source="' +
                    self.ref_id(source_tilts_id) + '"/>')
        self.writel(S_GEOM, 3, '</control_vertices>')
        self.writel(S_GEOM, 2, '</spline>')
        self.writel(S_GEOM, 1, '</geometry>')

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
                    matrix = pose_parent.matrix.inverted() @ (armature.matrix_world.inverted()
                                                              @ node.matrix_world)
        else:
            matrix = node.matrix_local.copy()
            if node.parent:
                if self.is_zero_scale(node.parent.matrix_local):
                    visible = False

        if (node.type == 'CAMERA'):
            m = Matrix.Rotation(-math.pi / 2.0, 4, Vector((1, 0, 0)))
            matrix = matrix @ m

        if (self.transform_matrix_scale):
            matrix, scale = self.split_matrix_scale(matrix)
            return {"matrix": matrix, "scale": scale}, visible
        else:
            return {"matrix": matrix}, visible

    def split_matrix_scale(self, matrix):
        m = matrix.normalized()
        m.translation = matrix.to_translation()
        return m, matrix.to_scale()

    def get_scale_xml(self, scale):
        return ['<scale sid="scale">' + self.strxyz(scale, True) + '</scale>']

    def get_matrix_transform_xml(self, matrix):
        return['<matrix sid="transform">' + self.strmtx(matrix) + '</matrix>']

    def export_node(self, section, node, il, recurse, lookup):
        # export a scene node as a Collada node

        prev_id = lookup["nodes"].get(node, None)
        if prev_id:
            # previously exported node is occurring again
            node_id = prev_id
            instance_prev_node = True
        else:
            node_id = self.gen_unique_id(node.name)
            lookup["nodes"][node] = node_id
            instance_prev_node = False

        instance_node = False

        if instance_prev_node:
            self.writel(section, il, '<instance_node url="{}">'.format(
                self.ref_id(node_id)))
            instance_node = True
            il += 1
        else:

            self.writel(section, il, '<node id="{}" name="{}" type="NODE">'.format(
                node_id, node.name))
            il += 1

            transforms = self.get_node_transform_xml(node)
            for t in transforms:
                self.writel(section, il, t)

            if (not node.is_instancer):
                if (node.type == "ARMATURE"):
                    self.export_armature_node(
                        section, il, node, recurse, lookup)
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
                elif (node in lookup["node_to_morph_controller"]):
                    morph_id = lookup["node_to_morph_controller"][node]
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
                    self.writel(section, il,
                                '<instance_camera url="' + self.ref_id(camera_id) + '"/>')
                elif (node.data in lookup["light"]):
                    light_id = lookup["light"][node.data]
                    self.writel(section, il,
                                '<instance_light url="' + self.ref_id(light_id) + '"/>')
            else:
                if node.instance_type == "COLLECTION":
                    self.writel(
                        section, il, '<instance_node url="{}"/>'.format(self.ref_library(node.instance_collection)))

        if recurse:
            for x in [child for child in node.children if child.parent_type != "BONE"]:
                self.export_node(section, x, il, recurse, lookup)

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

    def export_material_bind(self, section, node, il, lookup):
        if not node.material_slots:
            return
        if not len([slot.material for slot in node.material_slots if slot.material]):
            return
        material_bind = lookup["node_to_mesh"][node].get(
            "material_bind", None)
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

    def export_materials(self, depsgraph, lookup):

        # get materials for export
        materials = {
            slot.material for n in self.visual_nodes
            if hasattr(n, "material_slots")
            for slot in n.material_slots if slot.material and not slot.material.library
            if slot.material not in lookup["material"]}

        # export library_effects content

        for mat in materials:
            effect_id = self.gen_unique_id(mat.name + "-effect")
            lookup["effect"][mat] = effect_id
            self.export_effect(mat, effect_id, lookup)

        # export library_materials content
        for mat in materials:
            material_id = self.gen_unique_id(mat.name + "-material")
            self.export_material(mat, material_id, lookup["effect"][mat])
            lookup["material"][mat] = material_id

    def external_ref(self, library, id):
        return "./" + os.path.splitext(urlparse(library.filepath).netloc)[0].replace("\\", "/") + ".dae" + self.ref_id(id)

    def export_material(self, material, material_id, effect_id):
        # Material
        self.writel(S_MATS, 1, '<material id="' + material_id +
                    '" name="' + material.name + '">')
        self.writel(S_MATS, 2, '<instance_effect url="' +
                    self.ref_id(effect_id) + '"/>')
        self.writel(S_MATS, 1, '</material>')

    def is_node_valid(self, node):
        if node == None:
            return False
        if (not hasattr(node, "data")):
            return False
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

    def export_physics_nodes(self, physics_nodes, lookup):
        self.export_physics_materials(physics_nodes, lookup)
        self.export_physics_rigid_body_models(physics_nodes, lookup)
        self.export_physics_scene(physics_nodes, lookup)

    def export_physics_materials(self, physics_nodes, lookup):
        for node in physics_nodes:
            if (not node.data in lookup["physics_material"]):
                physics_material_id = self.gen_unique_id(
                    node.data.name + '-phys_mat')
                lookup["physics_material"][node.data] = physics_material_id
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
        for node in physics_nodes:
            if (not node.data in lookup["physics_rigid_body"]):
                physics_model_id = self.gen_unique_id(
                    node.data.name + '-model')
                physics_body_sid = self.gen_unique_id(node.data.name + '-body')
                lookup["physics_rigid_body"][node.data] = {
                    'model_id': physics_model_id, 'body_sid': physics_body_sid}
                self.export_physics_rigid_body_model(
                    node, physics_model_id, physics_body_sid, lookup)

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
        self.writel(S_P_MODEL, 5, '<instance_physics_material url="{}"/>'.format(
            self.ref_id(lookup['physics_material'][node.data])))
        self.shape_funcs[node.rigid_body.collision_shape](node, 5, lookup)
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
        collision_collections = [str(i) for (i, g) in enumerate(
            node.rigid_body.collision_collections) if g]
        if (len(collision_collections)):
            self.writel(
                S_P_MODEL, 5, '<collision_filter_groups>{}</collision_filter_groups>'.format(" ".join(collision_collections)))
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

        physics_scene_id = self.gen_unique_id(
            self.bpy_context_scene.name + '-physics')
        self.writel(
            S_P_SCENE, 1, '<physics_scene id="{}">'.format(physics_scene_id))
        self.writel(S_P_SCENE, 2, '<technique_common>')
        self.writel(
            S_P_SCENE, 3, '<gravity>{}</gravity>'.format(self.strxyz(self.bpy_context_scene.gravity)))
        self.writel(S_P_SCENE, 3, '<time_step>{}</time_step>'
                    .format(1.0 / self.bpy_context_scene.rigidbody_world.substeps_per_frame))
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
            sid = self.gen_unique_id(
                node.name + '-' + physics_model['model_id'])
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
        return [d / s for (d, s) in zip(node.dimensions, node.scale)]

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
        return lookup["convex_mesh"].get(node, None)

    def export_convex_hull_shape(self, node, il, lookup):
        mesh_id = self.get_physics_mesh_id(node, lookup)
        if (mesh_id != None):
            self.writel(S_P_MODEL, il,
                        '<instance_geometry url="{}"/>'.format(self.ref_id(mesh_id)))
        else:
            self.writel(
                S_P_MODEL, il, '<convex_mesh convex_hull_of="{}"/>'
                .format(self.ref_id(lookup["node_to_mesh"][node]["id"])))

    def export_mesh_shape(self, node, il, lookup):
        convex, mesh_id = self.get_physics_mesh_id(node, lookup)
        self.writel(
            S_P_MODEL, il, '<instance_geometry url="{}"/>'.format(self.ref_id(mesh_id)))

    def export_collections(self, lookup):

        for collection in bpy.data.collections:
            if len(collection.users_dupli_group) == 0:
                # don't export unused collections
                continue
            collection_id = self.gen_unique_id(collection.name)
            self.writel(S_LIBRARY_NODES, 1, '<node id="{}" name="{}" sid="COLLECTION" type="NODE">'
                        .format(collection_id, collection.name))
            for obj in collection.objects:
                self.export_node(S_LIBRARY_NODES, obj, 2, False, lookup)
            self.writel(S_LIBRARY_NODES, 1, '</node>')

    def export_scene(self, lookup):

        visual_scene_id = self.gen_unique_id(self.bpy_context_scene.name)
        self.writel(S_NODES, 1, '<visual_scene id="' + visual_scene_id +
                    '" name="' + self.bpy_context_scene.name + '">')

        for obj in self.visual_nodes:
            if (obj.parent == None):
                self.export_node(S_NODES, obj, 2, True, lookup)

        self.writel(S_NODES, 1, '</visual_scene>')

    def export_asset(self):
        self.writel(S_ASSET, 0, '<asset>')
        self.writel(S_ASSET, 1, '<contributor>')
        self.writel(S_ASSET, 2, '<author> Anonymous </author>')
        self.writel(
            S_ASSET, 2, '<authoring_tool> Collada Exporter for Blender 2.9+, by Gregery Barton </authoring_tool>')
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
        target_weight = target[target.find(":")+1:]
        anim_id = self.gen_unique_id(
            action_name + "-" + target_weight[:target_weight.find("/")]+"-"+target[:target.find(":")])

        self.writel(S_ANIM, 1, '<animation id="' + anim_id + '">')

        source_frames = " ".join([strflt(k[0]) for k in keys])
        source_value = " ".join([strflt(k[1]) for k in keys])
        source_interps = " ".join([(" LINEAR ") * len(keys)])

        source_input_id = self.gen_unique_id(anim_id + '-input')
        self.writel(S_ANIM, 2, '<source id="' + source_input_id+'">')
        array_input_id = self.gen_unique_id(anim_id + '-input-array')
        self.writel(S_ANIM, 3, '<float_array id="' + array_input_id+'" count="' +
                    str(frame_total) + '">' + source_frames + '</float_array>')
        self.writel(S_ANIM, 3, '<technique_common>')
        self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(array_input_id) +
                    '" count="' + str(frame_total) + '" stride="1">')
        self.writel(S_ANIM, 5, '<param name="TIME" type="float"/>')
        self.writel(S_ANIM, 4, '</accessor>')
        self.writel(S_ANIM, 3, '</technique_common>')
        self.writel(S_ANIM, 2, '</source>')

        # Value Source
        source_weights_output_id = self.gen_unique_id(
            anim_id + '-weights-output')
        self.writel(S_ANIM, 2, '<source id="' +
                    source_weights_output_id + '">')
        array_weights_output_id = self.gen_unique_id(
            anim_id + '-weights-output-array')
        self.writel(S_ANIM, 3, '<float_array id="' + array_weights_output_id+'" count="' +
                    str(frame_total) + '">' + source_value + '</float_array>')
        self.writel(S_ANIM, 3, '<technique_common>')
        self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(array_weights_output_id) +
                    '" count="' + str(frame_total) + '" stride="1">')
        self.writel(S_ANIM, 5, '<param name="X" type="float"/>')
        self.writel(S_ANIM, 4, '</accessor>')
        self.writel(S_ANIM, 3, '</technique_common>')
        self.writel(S_ANIM, 2, '</source>')

        # Interpolation Source

        source_interpolation_id = self.gen_unique_id(
            anim_id+'-interpolation-output')
        self.writel(S_ANIM, 2, '<source id="' + source_interpolation_id + '">')
        array_interpolation_id = self.gen_unique_id(
            anim_id + '-interpolation-output-array')
        self.writel(S_ANIM, 3, '<Name_array id="' + array_interpolation_id+'" count="' +
                    str(frame_total) + '">' + source_interps + '</Name_array>')
        self.writel(S_ANIM, 3, '<technique_common>')
        self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(array_interpolation_id) +
                    '" count="' + str(frame_total) + '" stride="1">')
        self.writel(S_ANIM, 5, '<param name="INTERPOLATION" type="Name"/>')
        self.writel(S_ANIM, 4, '</accessor>')
        self.writel(S_ANIM, 3, '</technique_common>')
        self.writel(S_ANIM, 2, '</source>')

        # weight sampler

        sampler_weights_id = self.gen_unique_id(anim_id + '-weights-sampler')
        self.writel(S_ANIM, 2, '<sampler id="' + sampler_weights_id + '">')
        self.writel(S_ANIM, 3, '<input semantic="INPUT" source="' +
                    self.ref_id(source_input_id) + '"/>')
        self.writel(S_ANIM, 3, '<input semantic="OUTPUT" source="' +
                    self.ref_id(source_weights_output_id) + '"/>')
        self.writel(S_ANIM, 3, '<input semantic="INTERPOLATION" source="' +
                    self.ref_id(source_interpolation_id) + '"/>')
        self.writel(S_ANIM, 2, '</sampler>')

        self.writel(S_ANIM, 2, '<channel source="' +
                    self.ref_id(sampler_weights_id) + '" target="' + target_weight + '"/>')
        self.writel(S_ANIM, 1, '</animation>')

        return anim_id

    def export_animation_xforms(self, target, action_name, keys):

        frame_total = len(keys)
        anim_id = self.gen_unique_id(action_name + "-" + target)

        self.writel(S_ANIM, 1, '<animation id="' + anim_id + '">')

        source_frames = " ".join([strflt(k[0]) for k in keys])
        source_matrix = " ".join([self.strmtx(k[1]['matrix']) for k in keys])
        if (self.transform_matrix_scale):
            source_scale = " ".join([self.strxyz(k[1]['scale']) for k in keys])

        source_interps = " ".join([(" LINEAR ") * len(keys)])

        # Time Source
        source_input_id = self.gen_unique_id(anim_id + '-input')
        self.writel(S_ANIM, 2, '<source id="' + source_input_id+'">')
        array_input_id = self.gen_unique_id(anim_id + '-input-array')
        self.writel(S_ANIM, 3, '<float_array id="'+array_input_id + '" count="' +
                    str(frame_total) + '">' + source_frames + '</float_array>')
        self.writel(S_ANIM, 3, '<technique_common>')
        self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(array_input_id) +
                    '" count="' + str(frame_total) + '" stride="1">')
        self.writel(S_ANIM, 5, '<param name="TIME" type="float"/>')
        self.writel(S_ANIM, 4, '</accessor>')
        self.writel(S_ANIM, 3, '</technique_common>')
        self.writel(S_ANIM, 2, '</source>')

        # Transform Source
        source_matrix_id = self.gen_unique_id(anim_id + '-matrix-output')
        self.writel(S_ANIM, 2, '<source id="'+source_matrix_id + '">')
        array_matrix_id = self.gen_unique_id(anim_id + '-matrix-output-array')
        self.writel(S_ANIM, 3, '<float_array id="' + array_matrix_id + '" count="' +
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
            source_scale_id = self.gen_unique_id(anim_id + '-scale-output')
            self.writel(S_ANIM, 2, '<source id="' + source_scale_id + '">')
            array_scale_id = self.gen_unique_id(
                anim_id + '-scale-output-array')
            self.writel(S_ANIM, 3, '<float_array id="' + array_scale_id+'" count="' +
                        str(frame_total * 3) + '">' + source_scale + '</float_array>')
            self.writel(S_ANIM, 3, '<technique_common>')
            self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(array_scale_id) +
                        '" count="' + str(frame_total) + '" stride="3">')
            self.writel(S_ANIM, 5, '<param name="X" type="float"/>')
            self.writel(S_ANIM, 5, '<param name="Y" type="float"/>')
            self.writel(S_ANIM, 5, '<param name="Z" type="float"/>')
            self.writel(S_ANIM, 4, '</accessor>')
            self.writel(S_ANIM, 3, '</technique_common>')
            self.writel(S_ANIM, 2, '</source>')

        # Interpolation Source

        source_interpolation_id = self.gen_unique_id(
            anim_id + '-interpolation-output')
        self.writel(S_ANIM, 2, '<source id="' + source_interpolation_id+'">')
        array_interpolation_id = self.gen_unique_id(
            anim_id + '-interpolation-output-array')
        self.writel(S_ANIM, 3, '<Name_array id="' + array_interpolation_id+'" count="' +
                    str(frame_total) + '">' + source_interps + '</Name_array>')
        self.writel(S_ANIM, 3, '<technique_common>')
        self.writel(S_ANIM, 4, '<accessor source="' + self.ref_id(
            array_interpolation_id) + '" count="' + str(frame_total) + '" stride="1">')
        self.writel(S_ANIM, 5, '<param name="INTERPOLATION" type="Name"/>')
        self.writel(S_ANIM, 4, '</accessor>')
        self.writel(S_ANIM, 3, '</technique_common>')
        self.writel(S_ANIM, 2, '</source>')

        sampler_matrix_id = self.gen_unique_id(anim_id + '-matrix-sampler')
        self.writel(S_ANIM, 2, '<sampler id="'+sampler_matrix_id + '">')
        self.writel(S_ANIM, 3, '<input semantic="INPUT" source="' +
                    self.ref_id(source_input_id) + '"/>')
        self.writel(S_ANIM, 3, '<input semantic="OUTPUT" source="' +
                    self.ref_id(source_matrix_id) + '"/>')
        self.writel(S_ANIM, 3, '<input semantic="INTERPOLATION" source="' +
                    self.ref_id(source_interpolation_id) + '"/>')
        self.writel(S_ANIM, 2, '</sampler>')

        if (self.transform_matrix_scale):
            sample_scale_id = self.gen_unique_id(anim_id + '-scale-sampler')
            self.writel(S_ANIM, 2, '<sampler id="' + sample_scale_id + '">')
            self.writel(S_ANIM, 3, '<input semantic="INPUT" source="' +
                        self.ref_id(source_input_id)+'"/>')
            self.writel(S_ANIM, 3, '<input semantic="OUTPUT" source="' +
                        self.ref_id(source_scale_id) + '"/>')
            self.writel(S_ANIM, 3, '<input semantic="INTERPOLATION" source="' +
                        self.ref_id(source_interpolation_id) + '"/>')
            self.writel(S_ANIM, 2, '</sampler>')

        self.writel(S_ANIM, 2, '<channel source="' +
                    self.ref_id(sampler_matrix_id) + '" target="' + target + '/transform"/>')
        if (self.transform_matrix_scale):
            self.writel(S_ANIM, 2, '<channel source="' +
                        self.ref_id(sample_scale_id) + '" target="' + target + '/scale"/>')
        self.writel(S_ANIM, 1, '</animation>')

        return anim_id

    def get_animation_transforms(self, start, end, lookup):

        # get a cache of the scene in rest pose to detect constant and symmetrical
        # animations

        rest_xform_cache = {}
        rest_blend_cache = {}
        self.rest_scene()
        self.cache_scene(rest_xform_cache, rest_blend_cache, 0, lookup)
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
            self.cache_scene(xform_cache, blend_cache, key, lookup)

        self.bpy_context_scene.frame_set(frame_orig)

        xform_cache = self.xform_cache_without_constants(
            rest_xform_cache, xform_cache)
        blend_cache = self.blend_cache_without_constants(
            rest_blend_cache, blend_cache)

        return xform_cache, blend_cache

    def cache_scene(self, xform_cache, blend_cache, key, lookup):
        # only animate a morph controller once, even if instanced by multiple nodes
        morph_controller_crumb = set()
        for orig_node in self.visual_nodes:
            node_id = lookup["nodes"][orig_node]
            node = orig_node.evaluated_get(
                bpy.context.evaluated_depsgraph_get())

            if orig_node in lookup["node_to_morph_controller"]:
                morph_id = lookup["node_to_morph_controller"][orig_node]

                if not morph_id in morph_controller_crumb:
                    morph_controller_crumb.add(morph_id)

                    for i in range(1, len(node.data.shape_keys.key_blocks)):
                        key_name = node.data.shape_keys.key_blocks[i].name
                        target = key_name+":" + morph_id + \
                            "/MORPH_WEIGHT_TO_TARGET("+str(i-1)+")"
                        if (not (target in blend_cache)):
                            blend_cache[target] = []
                        self.append_morph_keyframe_if_different(
                            blend_cache[target], key, node.data.shape_keys.key_blocks[i].value)

            if (node.type == "ARMATURE"):
                # All bones exported for now

                for bone in node.data.bones:
                    bone_node_id = lookup["skeleton_info"][orig_node].get(
                        bone.name, None)
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
        self.writel(S_ANIM_CLIPS, 1, '<animation_clip id="' + id + '" start="' +
                    strflt((start - 1) / self.bpy_context_scene.render.fps) + '" end="' + strflt((end - 1) / self.bpy_context_scene.render.fps) + '">')
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
        for node_id, cache in xform_cache.items():
            tcn.append(self.export_animation_xforms(
                node_id, action_name, cache))
        for target_id, cache in blend_cache.items():
            tcn.append(self.export_animation_blends(
                target_id, action_name, cache))
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
            start = 0
            end = 0
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
            start = 0
            end = 0
        return int(start), int(end)

    def export_animations(self, lookup):

        if self.config["use_anim_timeline"]:
            timeline_anim_id = self.gen_unique_id(
                self.bpy_context_scene.name + "-timeline")
            self.export_timeline(timeline_anim_id, self.bpy_context_scene.frame_start,
                                 self.bpy_context_scene.frame_end, lookup)

        if self.config["clip_type"] != 'NONE':
            self.mute_timeline()
            nla = self.get_NLA_objects()

            try:
                if self.config["clip_type"] == 'OBJECT':
                    for node in nla.keys():
                        self.mute_NLA(nla)
                        self.unmute_NLA_object(node)
                        start, end = self.get_NLA_object_timeline(node)
                        self.export_timeline(self.gen_unique_id(
                            node.name), start, end, lookup)

                if self.config["clip_type"] == 'TRACK':
                    for tracks in nla.values():
                        for track in tracks:
                            self.mute_NLA(nla)
                            self.unmute_NLA_track(track[0])
                            start, end = self.get_NLA_track_timeline(track[0])
                            self.export_timeline(self.gen_unique_id(
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
                                self.export_timeline(self.gen_unique_id(
                                    strip[0].name), start, end, lookup)
                                strip[0].mute = True
                            track[0].mute = True

            finally:
                self.restore_NLA(nla)
                self.unmute_timeline()

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
            # [mesh]=mesh-convex id
            "convex_mesh": {},
            # [camera(data)]=  id
            "camera": {},
            # [light(data)]=id
            "light": {},
            # mesh data to list of [poly group index, target symbol name]
            "node_to_skin": {},
            # [object]= id
            "nodes": {},
            # [armature node][bone]=joint id
            "skeleton_info": {},
            # [node.data/node.game]=id
            "physics_material": {},
            "physics_rigid_body": {},
            # node to generated mesh
            "node_to_mesh": {},
            # node.data to generated mesh
            "data_to_mesh": {},
            # [node] = [morph_controller_id]
            "node_to_morph_controller": {},
            # [node.data] = [morph_controller_id]
            "data_to_morph_controller": {},
        }

        self.bpy_context_scene = bpy.context.scene

        self.export_asset()

        scenes = [scene for scene in bpy.data.scenes]

        for scene in scenes:
            try:
                self.bpy_context_scene = scene

                self.get_visual_nodes()
                self.save_scene_pose()
                self.rest_scene()
                depsgraph = bpy.context.evaluated_depsgraph_get()

                self.export_materials(depsgraph, lookup)
                self.export_meshes(depsgraph, lookup)
                self.export_cameras(lookup)
                self.export_lights(lookup)
                self.export_morph_controllers(depsgraph, lookup)
                self.export_skin_controllers(depsgraph, lookup)
                self.export_collections(lookup)
                self.export_scene(lookup)

                physics_nodes = [node
                                 for node in self.visual_nodes
                                 if (node.rigid_body and node.rigid_body.collision_shape)]
                self.export_physics_nodes(physics_nodes, lookup)

            finally:
                self.restore_scene_pose()

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
                          self.ref_id(scene.name) + '"/>\n', "UTF-8"))
            if (self.bpy_context_scene.rigidbody_world):
                f.write(bytes('\t<instance_physics_scene url="' +
                        self.ref_id(scene.name) + '-physics' + '"/>\n', "UTF-8"))
            f.write(bytes('</scene>\n', "UTF-8"))
            f.write(bytes('</COLLADA>\n', "UTF-8"))
        finally:
            f.close()

        return True

    def __init__(self, path, kwargs, operator):
        self.operator = operator
        self.last_id = 0
        self.sections = {}
        self.path = path
        self.config = kwargs
        self.visual_nodes = set()
        self.node_ids = set()
        self.transform_matrix_scale = self.config["transform_type"] == 'MATRIX_SCALE'
        self.image_cache = set()
        self.axis_type = self.config['axis_type']
        self.use_tangents = self.config['calc_tangents']
        self.overstuff_bones = False
        self.use_active_layers = False


def save(operator, context,
         filepath="",
         use_selection=False,
         **kwargs
         ):

    exp = DaeExporter(filepath, kwargs, operator)
    exp.export()
    return {'FINISHED'}  # so the script wont run after we have batch exported.
