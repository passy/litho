/*
 * Copyright 2014-present Facebook, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.facebook.litho.widget;

/**
 * Utilities for sections debugging. Note: this should probably be in the sections package, but that
 * package has a dependency on the widget package where RecyclerBinder lives.
 */
public class SectionsDebug {

  public static final boolean ENABLED = false;
  public static final String TAG = "SectionsDebug";
}