import bpy
import math
import mathutils
from mathutils import Matrix

def createArm():
    line1 = [(0.11, 0.35, 1), (0.16, 0.33, 1), (0.23, 0.35, 0.99), (0.3, 0.38, 0.96), (0.35, 0.43, 0.9), (0.29, 0.42, 0.8), (0.22, 0.37,0.7), (0.14, 0.34, 0.62), (0.078, 0.296, 0.585)]
    line2 = [(0, 0, 1), (0.13, 0.09, 1), (0.29, 0.22, 0.99), (0.4, 0.33, 0.95), (0.41, 0.45, 0.88), (0.31, 0.47, 0.77), (0.2, 0.43, 0.65), (0.08, 0.4, 0.56), (-0.019, 0.398, 0.526)]
    line3 = [(0.36, 0, 1), (0.39, 0.11, 1), (0.45, 0.23,0.99), (0.49, 0.35, 0.95), (0.47, 0.45, 0.86), (0.36, 0.52, 0.73), (0.22, 0.5, 0.59), (0.13, 0.48, 0.48), (0.07, 0.489, 0.437)]

    vertices = []
    faces = []

    for i in range(len(line1)):
        vc = len(vertices)
        vertices.append(line1[i])
        vertices.append(line2[i])    
        vertices.append(line3[i])    
        if vc > 0:
            faces.append([vc-3, vc-3+1, vc+1, vc])
            faces.append([vc-3+1, vc-3+2, vc+2, vc+1])

    armmesh = bpy.data.meshes.new("ArmMesh")
    armmesh.from_pydata(vertices, [], faces)
    armmesh.update()

    arm = bpy.data.objects.new("Arm", armmesh)
    return arm

def rotate(rx, ry, rz):
    rotation_z1 = Matrix.Rotation(math.pi - rz, 4, 'Z')
    rotation_x = Matrix.Rotation(ry, 4, 'X')
    rotation_z2 = Matrix.Rotation(math.pi - rx, 4, 'Z')
    mat = rotation_z1 @ rotation_x @ rotation_z2
    return mat

def createStar(name, armTemplate, mStar, collection):
    for i in range(5):
        psi = i * 0.4 * math.pi
        arm = armTemplate.copy()        
        mArm  = rotate(0, 0, psi)
        arm.matrix_world @= mStar @ mArm
        arm.name = name + "_Arm_" + str(i)
        collection.objects.link(arm)
    
arm = createArm()

collection = bpy.data.collections.new("Quintrino")
bpy.context.scene.collection.children.link(collection)

createStar("Star_Top", arm, Matrix.Identity(4), collection)

alpha = math.acos(-math.sqrt(5)/5)  # Dihedral angle, ~116.57°
for i in range(5):
    psi = i * 0.4 * math.pi
    createStar("Star_Upper_" + str(i),arm, rotate(0, math.pi - alpha, psi + math.pi/5), collection)

for i in range(5):
    psi = i * 0.4 * math.pi
    createStar("Star_Lower_" + str(i), arm, rotate(math.pi/5, alpha, psi), collection)

createStar("Star_Bottom", arm, rotate(0, math.pi, 0), collection)




