/*
 * Copyright (c) 2017-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.facebook.litho.specmodels.generator;

import static org.assertj.core.api.Java6Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.facebook.litho.annotations.OnCreateTreeProp;
import com.facebook.litho.annotations.Prop;
import com.facebook.litho.annotations.State;
import com.facebook.litho.specmodels.internal.ImmutableList;
import com.facebook.litho.specmodels.model.ClassNames;
import com.facebook.litho.specmodels.model.DelegateMethod;
import com.facebook.litho.specmodels.model.MethodParamModelFactory;
import com.facebook.litho.specmodels.model.SpecMethodModel;
import com.facebook.litho.specmodels.model.SpecModel;
import com.facebook.litho.specmodels.model.TreePropModel;
import com.squareup.javapoet.AnnotationSpec;
import com.squareup.javapoet.ParameterizedTypeName;
import com.squareup.javapoet.TypeName;
import com.squareup.javapoet.TypeVariableName;
import java.lang.annotation.Annotation;
import java.util.ArrayList;
import javax.lang.model.element.Modifier;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests {@link TreePropGenerator}
 */
public class TreePropGeneratorTest {
  private final SpecModel mSpecModel = mock(SpecModel.class);
  private final SpecModel mGenericSpecModel = mock(SpecModel.class);
  private final TreePropModel mTreeProp = mock(TreePropModel.class);
  private final TreePropModel mGenericTreeProp = mock(TreePropModel.class);
  private SpecMethodModel<DelegateMethod, Void> mOnCreateTreePropMethodModel;
  private SpecMethodModel<DelegateMethod, Void> mGenericOnCreateTreePropMethodModel;

  @Before
  public void setUp() {
    mOnCreateTreePropMethodModel =
        new SpecMethodModel<DelegateMethod, Void>(
            ImmutableList.<Annotation>of(
                new OnCreateTreeProp() {

                  @Override
                  public Class<? extends Annotation> annotationType() {
                    return OnCreateTreeProp.class;
                  }
                }),
            ImmutableList.of(Modifier.PROTECTED),
            "onCreateTreeProp",
            TypeName.BOOLEAN,
            ImmutableList.<TypeVariableName>of(),
            ImmutableList.of(
                MethodParamModelFactory.create(
                    ClassNames.COMPONENT_CONTEXT,
                    "componentContext",
                    new ArrayList<Annotation>(),
                    new ArrayList<AnnotationSpec>(),
                    new ArrayList<Class<? extends Annotation>>(),
                    true,
                    null),
                MethodParamModelFactory.create(
                    TypeName.BOOLEAN,
                    "prop",
                    ImmutableList.of(createAnnotation(Prop.class)),
                    new ArrayList<AnnotationSpec>(),
                    new ArrayList<Class<? extends Annotation>>(),
                    true,
                    null),
                MethodParamModelFactory.create(
                    TypeName.INT,
                    "state",
                    ImmutableList.of(createAnnotation(State.class)),
                    new ArrayList<AnnotationSpec>(),
                    new ArrayList<Class<? extends Annotation>>(),
                    true,
                    null)),
            null,
            null);

    when(mTreeProp.getName()).thenReturn("treeProp");
    when(mTreeProp.getTypeName()).thenReturn(TypeName.INT);

    when(mSpecModel.getContextClass()).thenReturn(ClassNames.COMPONENT_CONTEXT);
    when(mSpecModel.getComponentClass()).thenReturn(ClassNames.COMPONENT);
    when(mSpecModel.getComponentName()).thenReturn("Test");
    when(mSpecModel.getSpecName()).thenReturn("TestSpec");
    when(mSpecModel.getDelegateMethods())
        .thenReturn(ImmutableList.of(mOnCreateTreePropMethodModel));
    when(mSpecModel.getTreeProps()).thenReturn(ImmutableList.of(mTreeProp));

    mGenericOnCreateTreePropMethodModel =
        new SpecMethodModel<DelegateMethod, Void>(
            ImmutableList.<Annotation>of(
                new OnCreateTreeProp() {

                  @Override
                  public Class<? extends Annotation> annotationType() {
                    return OnCreateTreeProp.class;
                  }
                }),
            ImmutableList.of(Modifier.PROTECTED),
            "onCreateTreeProp",
            ParameterizedTypeName.get(GenericObject.class, Integer.class),
            ImmutableList.<TypeVariableName>of(),
            ImmutableList.of(
                MethodParamModelFactory.create(
                    ClassNames.COMPONENT_CONTEXT,
                    "componentContext",
                    new ArrayList<Annotation>(),
                    new ArrayList<AnnotationSpec>(),
                    new ArrayList<Class<? extends Annotation>>(),
                    true,
                    null),
                MethodParamModelFactory.create(
                    ParameterizedTypeName.get(GenericObject.class, Integer.class),
                    "prop",
                    ImmutableList.of(createAnnotation(Prop.class)),
                    new ArrayList<AnnotationSpec>(),
                    new ArrayList<Class<? extends Annotation>>(),
                    true,
                    null)),
            null,
            null);

    when(mGenericTreeProp.getName()).thenReturn("genericTreeProp");
    when(mGenericTreeProp.getTypeName())
        .thenReturn(ParameterizedTypeName.get(GenericObject.class, Boolean.class));

    when(mGenericSpecModel.getContextClass()).thenReturn(ClassNames.COMPONENT_CONTEXT);
    when(mGenericSpecModel.getComponentClass()).thenReturn(ClassNames.COMPONENT);
    when(mGenericSpecModel.getComponentName()).thenReturn("Test");
    when(mGenericSpecModel.getSpecName()).thenReturn("TestSpec");
    when(mGenericSpecModel.getDelegateMethods())
        .thenReturn(ImmutableList.of(mGenericOnCreateTreePropMethodModel));
    when(mGenericSpecModel.getTreeProps())
        .thenReturn(ImmutableList.of(mGenericTreeProp, mTreeProp));
  }

  @Test
  public void testGenerate() {
    TypeSpecDataHolder typeSpecDataHolder =
        TreePropGenerator.generate(mSpecModel);

    assertThat(typeSpecDataHolder.getFieldSpecs()).isEmpty();
    assertThat(typeSpecDataHolder.getMethodSpecs()).hasSize(2);
    assertThat(typeSpecDataHolder.getTypeSpecs()).isEmpty();

    assertThat(typeSpecDataHolder.getMethodSpecs().get(0).toString()).isEqualTo(
        "@java.lang.Override\n" +
            "protected void populateTreeProps(com.facebook.litho.TreeProps treeProps) {\n" +
            "  if (treeProps == null) {\n" +
            "    return;\n" +
            "  }\n" +
            "  treeProp = treeProps.get(int.class);\n" +
            "}\n");

    assertThat(typeSpecDataHolder.getMethodSpecs().get(1).toString()).isEqualTo(
        "@java.lang.Override\n" +
        "protected com.facebook.litho.TreeProps getTreePropsForChildren(com.facebook.litho.ComponentContext c,\n" +
        "    com.facebook.litho.TreeProps parentTreeProps) {\n" +
        "  final com.facebook.litho.TreeProps childTreeProps = com.facebook.litho.TreeProps.copy(parentTreeProps);\n" +
        "  childTreeProps.put(boolean.class, TestSpec.onCreateTreeProp(\n" +
        "      (com.facebook.litho.ComponentContext) c,\n" +
        "      prop,\n" +
        "      mStateContainer.state));\n" +
        "  return childTreeProps;\n" +
        "}\n");
  }

  @Test
  public void testGenericGenerate() {
    TypeSpecDataHolder typeSpecDataHolder = TreePropGenerator.generate(mGenericSpecModel);

    assertThat(typeSpecDataHolder.getFieldSpecs()).isEmpty();
    assertThat(typeSpecDataHolder.getMethodSpecs()).hasSize(2);
    assertThat(typeSpecDataHolder.getTypeSpecs()).isEmpty();

    assertThat(typeSpecDataHolder.getMethodSpecs().get(0).toString())
        .isEqualTo(
            "@java.lang.Override\n"
                + "protected void populateTreeProps(com.facebook.litho.TreeProps treeProps) {\n"
                + "  if (treeProps == null) {\n"
                + "    return;\n"
                + "  }\n"
                + "  genericTreeProp = treeProps.get(com.facebook.litho.specmodels.generator.TreePropGeneratorTest.GenericObject.class);\n"
                + "  treeProp = treeProps.get(int.class);\n"
                + "}\n");

    assertThat(typeSpecDataHolder.getMethodSpecs().get(1).toString())
        .isEqualTo(
            "@java.lang.Override\n"
                + "protected com.facebook.litho.TreeProps getTreePropsForChildren(com.facebook.litho.ComponentContext c,\n"
                + "    com.facebook.litho.TreeProps parentTreeProps) {\n"
                + "  final com.facebook.litho.TreeProps childTreeProps = com.facebook.litho.TreeProps.copy(parentTreeProps);\n"
                + "  childTreeProps.put(com.facebook.litho.specmodels.generator.TreePropGeneratorTest.GenericObject.class, TestSpec.onCreateTreeProp(\n"
                + "      (com.facebook.litho.ComponentContext) c,\n"
                + "      prop));\n"
                + "  return childTreeProps;\n"
                + "}\n");
  }

  private static Annotation createAnnotation(final Class<? extends Annotation> annotationClass) {
    return new Annotation() {
      @Override
      public Class<? extends Annotation> annotationType() {
        return annotationClass;
      }
    };
  }

  private static class GenericObject<T> {
    T value;
  }
}
