/* BSD 2-Clause License:
 * Copyright (c) 2009 - 2017
 * Software Technology Group
 * Department of Computer Science
 * Technische Universität Darmstadt
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package annotations.target;

import java.lang.annotation.*;
import static java.lang.annotation.RetentionPolicy.*;
import static java.lang.annotation.ElementType.*;

/**
 * Describes a method call. For types see the {@link TargetResolution} enum.
 * 
 * @author Arne Lottmann
 * @author Michael Reif
 */
@Retention(RUNTIME)
@Target(METHOD)
@Repeatable(InvokedMethods.class)
public @interface InvokedMethod {

	TargetResolution resolution() default TargetResolution.DEFAULT;

	/**
	 * The type name of the receiver using JVM notation (e.g.,
	 * "java/lang/Object").
	 */
	String receiverType();

	String name();

	Class<?> returnType() default Void.class;

	Class<?>[] parameterTypes() default {};

	int line() default -1;

	boolean isStatic() default false;

	boolean isReflective() default false;

	CallGraphAlgorithm[] isContainedIn() default { 
			CallGraphAlgorithm.CHA,
			CallGraphAlgorithm.BasicVTA,
			CallGraphAlgorithm.DefaultVTA,
			CallGraphAlgorithm.ExtVTA,
			CallGraphAlgorithm.CFA   };
}
