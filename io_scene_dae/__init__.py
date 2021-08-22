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

# <pep8-80 compliant>

from bpy_extras.io_utils import (ExportHelper,
                                 path_reference_mode,
                                 axis_conversion,
                                 )
from bpy.props import StringProperty, BoolProperty, FloatProperty, EnumProperty
import bpy
bl_info = {
    "name": "Python Collada Exporter",
    "author": "Gregery Barton",
    "version": (2, 0, 1),
    "blender": (2, 90, 0),
    "location": "File > Import-Export",
    "description": ("Export DAE Scenes."),
    "warning": "",
    "wiki_url": ("https://github.com/gregeryb/DAEBlend"),
    "tracker_url": "",
    "support": 'COMMUNITY',
    "category": "Import-Export"}

if "bpy" in locals():
    import imp
    if "export_dae" in locals():
        imp.reload(export_dae)


class ExportDAE(bpy.types.Operator, ExportHelper):
    '''Export to DAE'''
    bl_idname = "export_scene.dae"
    bl_label = "Export DAE"
    bl_options = {'PRESET'}

    filename_ext = ".dae"
    filter_glob: StringProperty(default="*.dae", options={'HIDDEN'})

    # List of operator properties, the attributes will be assigned
    # to the class instance from the operator settings before calling.

    use_export_selected: BoolProperty(
        name="Selected Objects",
        description="Export only selected objects (and visible in active layers if that applies).",
        default=False,
    )

    axis_type: EnumProperty(
        name="Coordinates",
        items=(('ZUP', "Z-Up (Blender)", "Z-Up axis, right handed"),
               ('YUPR', "Y-Up (OpenGL)", "Y-Up axis, right handed"),
               ('YUPL', "Y-Up (DirectX)",
                "Y-Up axis, left handed, Z-axis will be reflected")
               ),
        description="Adjust up axis. All geometry data is adjusted to the specified coordinate system.",
        default='YUPR',
    )

    transform_type: EnumProperty(
        name="Transforms",
        items=(('MATRIX', "Matrix only", "Transforms are in a single matrix"),
               ('MATRIX_SCALE', "Matrix+Scale",
                "Matrix with normalized rotation and  separate scale transform")
               ),
        description="Types of node transformations to use "
        "(Use 'Matrix only' to simplify the transforms, use 'Matrix+Scale' if the scene is for use in a physics simulation)",
        default='MATRIX',
    )

    calc_tangents: BoolProperty(
        name="Tangents",
        description="Generate tangents for vertices. Used for bump mapping.",
        default=False,
    )

    # use_copy_images: BoolProperty(
    #     name="Copy Images",
    #     description="Copy Images (create images/ subfolder)",
    #     default=True,
    # )

    use_anim_timeline: BoolProperty(
        name="Timeline",
        description=("Export the main timeline animation"),
        default=True,
    )

    clip_type: EnumProperty(
        name="NLA Clips",
        items=(('NONE', "None", "No animation clips"),
               ('OBJECT', "Objects", "Blended tracks for each object in the NLA editor"),
               ('TRACK', "Tracks", "A clip for each NLA track"),
               ('STRIP', "Strips", "A clip for each action on the NLA tracks")),
        description="Style of animation clips",
        default='NONE',
    )
    
    def check_extension(self):
        return True  # return self.batch_mode == 'OFF'

    def check(self, context):
        return True

    def execute(self, context):
        if not self.filepath:
            raise Exception("filepath not set")

        keywords = self.as_keywords(ignore=("axis_forward",
                                            "axis_up",
                                            "global_scale",
                                            "check_existing",
                                            "filter_glob",
                                            "xna_validate",
                                            ))

        from . import export_dae
        return export_dae.save(self, context, **keywords)


def menu_func(self, context):
    self.layout.operator(ExportDAE.bl_idname, text="Collada (Python script) (.dae)")


def register():
    bpy.utils.register_class(ExportDAE)

    bpy.types.TOPBAR_MT_file_export.append(menu_func)


def unregister():
    bpy.utils.unregister_class(ExportDAE)

    bpy.types.TOPBAR_MT_file_export.remove(menu_func)


if __name__ == "__main__":
    register()
